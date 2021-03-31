#!/usr/bin/env python3
'''Pavlok Bytecode Compiler'''

from collections import defaultdict
from fractions import Fraction
import itertools
import logging
import math
import re
import struct
import sys
import textwrap
import time
import traceback as tb

_missing = set()
try:    from zmq.utils import z85
except ImportError: _missing.add('pyzmq')

try:    import parsy
except ImportError: _missing.add('parsy')

if _missing:
    sys.exit(f'Install missing package(s) with: pip install {" ".join(_missing)}')


debug = False

def log_print(*args, **kwargs):
    logging.debug(*args, **kwargs)

log = log_print if debug else lambda *x: None


def as_hex_escapes(data):
    return ''.join(fr'\x{x:02x}' for x in data)


# Copied from nordicsemi/dfu/crc16.py
def calc_crc16(binary_data, crc=0xffff):
    """
    Calculates CRC16 on binary_data

    :param int crc: CRC value to start calculation with
    :param bytearray binary_data: Array with data to run CRC16 calculation on
    :return int: Calculated CRC value of binary_data
    """
    for b in binary_data:
        crc = (crc >> 8 & 0x00FF) | (crc << 8 & 0xFF00)
        crc ^= (b)      # original used ord(b), for Python 2
        crc ^= (crc & 0x00FF) >> 4
        crc ^= (crc << 8) << 4
        crc ^= ((crc & 0x00FF) << 4) << 1
    return crc & 0xFFFF


OCTAVE_SIZE = 12    # semitones per octave
OCTAVE_MIN = 0
OCTAVE_MAX = 9
NOTE_NAMES = 'C C# D D# E F F# G G# A A# B'.split()


def tone_name(code):
    '''Return tone name e.g. 61 is C#4'''
    octave = (code // OCTAVE_SIZE) - 1
    return f'{NOTE_NAMES[code%OCTAVE_SIZE]}{octave}'


class MusicItem:
    pass


class Operation(MusicItem):
    def __init__(self, cmd, *args):
        self.command = cmd
        self.args = args


    def __repr__(self):
        if self.args:
            return f'<Oper {self.command} = {self.args[0]}>'
        else:
            return f'<Oper {self.command}>'


class Playable(MusicItem):
    pass


class Note(Playable):
    def __init__(self, tone, duration, src=''):
        self.tone = tone
        self.duration = duration
        self.src = src
        # print('Note', self.duration, self.tone)

    def __repr__(self):
        stext = f' ("{self.src}")' if self.src else ''
        return f'<Note {self.duration} {tone_name(self.tone)}{stext}>'


class Rest(Playable):
    def __init__(self, duration, src=''):
        self.duration = duration
        self.src = src
        # print('Rest', self.duration)


    def __repr__(self):
        stext = f' ("{self.src}")' if self.src else ''
        return f'<Rest {self.duration}{stext}>'


class Music:
    '''Defines a piece of music to be encoded by the MusicEncoder.
    '''
    def __init__(self):
        self.items = []


    def add(self, item):
        self.items.append(item)


    def set_tempo(self, bpm):
        self.items.append(Operation('set tempo', bpm))


    def set_volume(self, value):
        self.items.append(Operation('set volume', value))



class MusicEncoder:
    '''Encode music contained in a Music object.

    Optimal encoding may not yet be achieved. Call it a goal.
    '''

    # predefined scale formulas
    PRESET_SCALES = [
        0b101011010101, # SCALE_MAJOR       0xad5 or #2741
        0b101101011010, # SCALE_MINOR       0xb5a or #1453
        0b101101011001, # SCALE_HARM_MINOR  0xb59 or #2477 Harmonic Minor
        0b101101101101, # SCALE_DIMINISHED  0xb6d or #2925
        0b101010010100, # SCALE_MAJOR_PENTA 0xa94 or #661
        0b100101010010, # SCALE_MINOR_PENTA 0x952 or #1193
        0b101001011000, # SCALE_JAPANESE    0xa58 or #421
        0b111111111111, # SCALE_CHROMATIC   0xFFF or #4095
        ]

    SCALE_NAMES = [
        'major',
        'minor',
        'h.minor',
        'dimin.',
        'p.major',
        'p.minor',
        'Japanese',
        'chromatic',
    ]

    # predefined duration values
    DEFAULT_DURATIONS = [
        Fraction(1, 16),
        Fraction(1, 8),
        Fraction(3, 16),
        Fraction(1, 4),
        Fraction(3, 8),
        Fraction(1, 2),
        Fraction(3, 4),
        Fraction(1, 1),
    ]

    MAX_DURATION_DENOM = 64

    # predefined parameters
    DEFAULT_SCALE = 0   # major
    DEFAULT_BASE  = 57  # A3
    DEFAULT_TONIC = 0   # C
    DEFAULT_VOLUME = 100 # 80 is -6dBFS (i.e. 6dB below full scale)
    DEFAULT_TEMPO = 100

    CODE_REST = 0x80
    CODE_SET_NOTE = 0x90
    CODE_ACCIDENTAL = 0xa0
    CODE_SET_TONIC = 0xb0
    CODE_SET_SCALE = 0xc0
    CODE_CUSTOM_SCALE = 0xc8
    CODE_SET_VOLUME = 0xd0
    CODE_SET_DURATION = 0xe0
    CODE_SET_TEMPO = 0xf0
    CODE_SET_OCTAVE = 0xf1

    NOTES_SIZE = 16


    def __init__(self, debug=False):
        self.debug = debug

        self.durations = {d: i for i, d in enumerate(self.DEFAULT_DURATIONS)}

        self.log = lambda *x: None


    def _fix_duration(self, dur):
        return dur.limit_denominator(self.MAX_DURATION_DENOM)


    def calc_item_stats(self):
        dur_counts = defaultdict(int)
        tone_counts = defaultdict(int)

        # scan and build histograms of duration and note
        for item in self.items:
            if isinstance(item, Playable):
                dur_counts[self._fix_duration(item.duration)] += 1

            if isinstance(item, Note):
                tone_counts[item.tone] += 1

        return {
            'duration_counts': dur_counts,
            'tone_counts': tone_counts,
        }


    def emit_durations(self, *durations):
        '''Emit code to reprogram specified durations.'''
        codes = []
        for i, duration in durations:
            duration *= 4   # scale to define fraction of a beat instead of whole note
            duration = duration.limit_denominator(32)
            if duration.numerator == 0:
                duration = Fraction(1, 32)  # smallest value 1/32 of 1/4 or 1/128

            yyyy = min(15, duration.numerator - 1)
            assert i < 8, f'internal error: duration entry {i} invalid'

            # E|yxxx+byte(s). set duration xxx to y+byte interpreted as yyyy/zzzzz
            #     times a quarter note (allows values up to 4 whole and down
            #     to 1/128 which we can't even quite achieve)
            codes.extend([self.CODE_SET_DURATION | (yyyy & 0b1000) | i,
                ((yyyy & 0b111) << 5) | (duration.denominator - 1)])

        dtext = ', '.join(f'{x}: {y.numerator}/{y.denominator}' for x, y in durations)
        self.log(f'emit_durations: {{{dtext}}} -> "{as_hex_escapes(bytes(codes))}"')
        self.code.extend(codes)


    def emit_set_note(self, x, tone, fixed=False):
        '''Emit code to set note0.'''

        # 9x+yy: redefine note x as tone code yy (fixed, if high bit set)
        codes = [self.CODE_SET_NOTE | (x & 0b1111), (tone&0x7f) | (0x80 if fixed else 0)]

        self.log(f'set note[{x}] = #{tone} -> "{as_hex_escapes(bytes(codes))}"')

        self.code.extend(codes)


    def emit_set_tonic(self, tonic):
        '''Emit code to set tonic.'''

        # Bx: set tonic (first note in scale)
        codes = [self.CODE_SET_TONIC | (tonic & 0b1111)]

        self.log(f'set tonic = {tonic} -> "{as_hex_escapes(bytes(codes))}"')

        self.code.extend(codes)


    def emit_set_scale(self, sidx):
        '''Emit code to select preset scale.'''

        # C|0xxx. set scale to predefined xxx
        codes = [self.CODE_SET_SCALE | (sidx & 0b0111)]

        self.log(f'set scale = {sidx} -> "{as_hex_escapes(bytes(codes))}"')

        self.code.extend(codes)


    def optimize_durations(self, d_hist):
        self.log('Durations used:\n' + '\n'.join(f'\t{x.numerator}/{x.denominator} = {y!r}' for x, y in
            sorted(d_hist.items(), key=lambda x: (x[1], x[0])) )
        )

        if len(d_hist) > len(self.durations):
            # breakpoint()
            raise ScriptError('too many durations (max 8 per piece)')

        undefined = set(d_hist.keys()) - set(self.durations)
        if undefined:
            new_durations = []
            self.log('Undefined durations:', ', '.join(
                f'{x.numerator}/{x.denominator}' for x in sorted(undefined)
                ))
            unused = set(self.durations) - set(d_hist.keys())
            for dur in undefined:
                override = unused.pop()
                self.durations[dur] = self.durations[override]
                del self.durations[override]
                new_durations.append((self.durations[dur], dur))

            self.emit_durations(*new_durations)

            self.log('Duration table:\n' + '\n'.join(f'\t{i}: {x.numerator}/{x.denominator}'
                for x, i in sorted(self.durations.items(), key=lambda x: x[1])
                ))


    def optimize_notes(self, t_hist):
        '''Check if we need to reprogram base note, scale, or tonic
        or reprogram any individual notes.
        '''
        # TODO: enhance this?  It's a very non-trivial problem.
        # * See if we can find a closely matching scale.
        # * Pick nearest standard scale that works adequately.
        # * Assign cost values based on various things like overhead of
        #   reprogramming to a custom scale or individual notes, or using
        #   accidentals during the piece.

        # First (crude) algorithm:
        # Scan each scale at each tonic starting with the default (major) scale.
        # Pick first one that includes all notes, if any.  Keep track of
        # closest one (or track all and sort), pick it, reassign unused notes
        # as extras in order by frequency of occurrence, use accidentals for
        # the rest. (This may cover all almost all regular music adequately.)
        # Assume we will reprogram the base note as required, but before we
        # do that check whether we can work with the default (A3).
        # For most stuff I'm guessing this will  result in us needing to change
        # only the tonic and/or scale, for a two byte overhead.
        self.log('Tones used:\n' + '\n'.join(f'\t{x:3d} = {y:3d}' for x, y in
            sorted(t_hist.items(), key=lambda x: (x[1], x[0])) )
        )

        tmin = min(t_hist or [self.DEFAULT_BASE])      # lowest tone used
        tmax = max(t_hist or [self.DEFAULT_BASE])      # highest tone used
        tspan = tmax - tmin + 1 # span covered by low to high tone
        tused = set(t_hist)     # set of all tones actually used

        # Note: could make use of numpy effectively here, but probably better
        # if we don't bring in that dependency just yet.
        note0 = tmin    # TODO: check with DEFAULT_BASE first...
        class Found(Exception): pass

        options = {}
        try:
            for sidx, scale in enumerate(self.PRESET_SCALES):
                for tonic in range(0, OCTAVE_SIZE):
                    # find all tones available for this tonic/scale combo
                    avail = self.get_tones(tonic, scale)
                    # shrink to those we could reach from note0
                    # Note: works only if note0 is actually in the set
                    # and note that for now, if it's not, we have undetermined
                    # behaviour. (Though it probably includes note0 as an
                    # exception to the scale pattern, then the next available
                    # regular note from the scale. Needs to be checked, and
                    # maybe standardized as-is, or changed.)
                    avail -= set(range(0, note0))
                    avail = sorted(avail)[:self.NOTES_SIZE]
                    atext = ' '.join(f'{x}/{tone_name(x)}' for x in avail)
                    # atext = ' '.join(f'{x}' for x in avail)
                    # print(f'{NOTE_NAMES[tonic]:2s} {self.SCALE_NAMES[sidx]:9s} = {atext}')

                    # TODO: actually pretty bad to just pick tmin as note0,
                    # since tmin could be way off by itself and we'd fail
                    # to cover most of the notes. Need to try something like
                    # sliding a 16 tone window along to find the best alignment.

                    # TODO: also, put some weight on how many times a given
                    # note is used?  There's a 2-byte overhead to redefine
                    # a note, but we save one byte each time it's used.
                    # But if we can cover all the frequent notes (say >1 usage)
                    # with a single span based on base/tonic/scale then
                    # there's only a 1-byte overhead for each single usage note
                    # that lies outside that.  Still better to realign to cover
                    # as many of them as possible, but until we have a real
                    # cost-based optimization going we could live without that.
                    # Hmm... but note that if we can cover 5 single usage notes
                    # with one alignment, leaving out a note used 50 times,
                    # that's better than picking an inferior alignment that
                    # covers the 50-times note but only 1 of the others.
                    # So the actual note count isn't that important, at least
                    # beyond a certain point. Maybe it's only important if we
                    # have to resort to using accidentals (since the 1-byte cost
                    # is per-note in that case), rather than when we're just
                    # trying to figure out the best base/tonic/scale values.
                    # Hmm.. and of course if we would need accidentals 50 times
                    # for one note, we're better off redefining some other note
                    # that would be covered automatically but is used only once
                    # or twice.

                    options[(note0, tonic, sidx)] = {
                        'unused': self.NOTES_SIZE - len(tused.intersection(avail)),
                        'missing': len(tused.difference(avail)),
                        'avail': set(avail),
                    }

                    if not tused.difference(avail):
                        raise Found(note0, tonic, sidx)

            self.log('Top matches:')
            top = sorted(options.items(), key=lambda x: x[1]['missing'])
            self.log('\n'.join(f'{i} -> {x}' for i, x in top[:10]
                ))

            raise Found(*top[0][0])

        except Found as ex:
            note0, tonic, sidx = ex.args
            stats = options[ex.args]
            match = '%smatch' % ('perfect ' if not stats['missing'] else '')
            self.log(f'Found {match}: {ex.args}')

        atext = ' '.join(f'{x}/{tone_name(x)}' for x in stats['avail'])
        self.log(f'Best is: {NOTE_NAMES[tonic]} {self.SCALE_NAMES[sidx]} = {atext}')

        if stats['missing']:
            stext = '\n'.join(f'\t{k} = {v}' for k, v in sorted(stats.items()))
            self.log(f'Stats:\n{stext}')

        if tonic != self.DEFAULT_TONIC:
            self.emit_set_tonic(tonic)

        if sidx != self.DEFAULT_SCALE:
            self.emit_set_scale(sidx)

        if note0 != self.DEFAULT_BASE:
            self.emit_set_note(0, note0)

        avail = stats['avail']
        self.notes = sorted(avail)
        undefined = tused - avail
        if undefined:
            # print('Note table:\n' + '\n'.join(f'\t{i}: {x}'
            #     for i, x in enumerate(self.notes)
            #     ))
            new_notes = []
            self.log('Undefined notes:', ', '.join(
                f'{x}' for x in sorted(undefined)
                ))
            unused = sorted(avail - tused)
            for note in undefined:
                try:
                    reuse = unused.pop(0)
                except IndexError:
                    # raise ScriptError('too many notes required (max 16 for now)')
                    self.log('ran out of unused notes, will use accidentals')
                    break

                idx = self.notes.index(reuse)
                self.notes[idx] = note
                new_notes.append((idx, note))

            for i, tone in new_notes:
                self.emit_set_note(i, tone)

            self.log('Note table:\n' + '\n'.join(f'\t{i}: {x}'
                for i, x in enumerate(self.notes)
                ))

        # Useful for testing:
        # 00000003_All_Songs_A2Z.txt 2MonthsO


    def get_tones(self, tonic, scale):
        '''Return a set including all tones that would be available for
        this tonic and scale combination, across the supported set of tones.
        '''
        # turn scale pattern into a set of offsets from tonic for only
        # the selected notes
        offsets = [i for i, x in enumerate(f'{scale:012b}') if x == '1']

        # TODO: start right down in lowest octave.  For now we just gloss over
        # the fact that if tonic is > 0 there may be a few notes that
        # we're not including between 0 and the tonic.  Tones 0 to 11 are pretty
        # irrelevant though, so not bothering for now.
        return set([b * OCTAVE_SIZE + tonic + offset
            for b in range(OCTAVE_MIN, OCTAVE_MAX)
                for offset in offsets
            ])


    def emit_music(self):
        # our second pass: emit code for each note and rest, using
        # accidentals where required for unmapped notes
        elapsed = 0.0
        tempo = self.DEFAULT_TEMPO
        codes = []
        notes = {x: i for i, x in enumerate(self.notes)}
        for i, item in enumerate(self.items):
            self.log(i, item)

            if isinstance(item, Note):
                # breakpoint()
                elapsed += 60 / tempo * 4 * item.duration
                duration = self.durations[item.duration]
                try:
                    note = notes[item.tone]
                    codes.append((duration << 4) | (note))
                except KeyError:
                    codes.append(self.CODE_ACCIDENTAL | (duration & 0b111))
                    codes.append(item.tone)

            elif isinstance(item, Rest):
                elapsed += 60 / tempo * 4 * item.duration
                duration = self.durations[item.duration]
                codes.append(self.CODE_REST | (duration & 0b111))

            elif isinstance(item, Operation):
                if item.command == 'set tempo':
                    codes.append(self.CODE_SET_TEMPO)
                    value = min(255, max(0, round((item.args[0] - 50) / 2)))
                    tempo = value * 2 + 50
                    codes.append(value)
                elif item.command == 'set volume':
                    codes.append(self.CODE_SET_VOLUME)
                    codes.append(item.args[0])
                else:
                    self.log(f'unrecognized: {item}')

            else:
                self.log('WTF?', item)
                raise ValueError(str(item))
                # breakpoint()

        self.log(f'Music duration: {elapsed:.1f}s')

        self.code.extend(codes)


    def encode(self, music):
        self.items = music.items

        self.code = []  # musicode generated into this array

        stats = self.calc_item_stats()

        self.optimize_durations(stats['duration_counts'])

        self.optimize_notes(stats['tone_counts'])

        self.emit_music()

        return self.code



# See also https://github.com/eriknyquist/ptttl for a Polyphonic extension to RTTTL.

# Class used primarily as a separate namespace to prevent the grammar
# from possible conflicts with other stuff, because it defines so many names.
# Also the unusual pattern with a function that's then called for a default
# argument is to let us define more complex parsers via @generate, which
# didn't seem possible within the class definition any other way because
# of whatever name scoping rules apply here.
class RttlGrammar:
    def construct():
        from parsy import (eof, regex, seq, string, string_from, whitespace, digit,
            char_from, generate)

        opt_white = whitespace.optional()
        sep = string(',')

        duration = regex(r'\d*')
        pitch = regex(r'(?i)[p]|([a-gh][#_]?|[#_]?[a-gh])')
        octave = regex(r'\d?')
        dot = regex(r'\.')

        @generate
        def _note():
            d = yield duration
            _dot = yield dot.optional()
            p = yield pitch
            if _dot:
                yield dot.should_fail('only one dot allowed')
            else:
                _dot = yield dot.optional()
            o = yield octave
            if _dot:
                yield dot.should_fail('only one dot allowed')
            else:
                _dot = yield dot.optional()
            return dict(d=d, p=p, o=o, dot=_dot)
        note = _note.mark().tag('note')
        # note = seq(d=duration, p=pitch, d1=dot1, o=octave, d2=dot2).mark().tag('note')

        name = (regex(r'[^:]*') << string(':')).tag('name')

        setting = seq(char_from('dobvDOBV') << regex(r'\s*=\s*'), regex(r'\d+')).mark().tag('setting')
        settings = ((opt_white >> setting << opt_white).sep_by(sep) << string(':')).tag('settings')

        notes = (opt_white >> ( setting | note ) << opt_white).sep_by(sep).tag('notes')  # need setting first since note can match a setting

        header = (name.optional() >> settings) | settings
        rttl = opt_white >> (seq(header, notes) | notes) << sep.optional() << opt_white

        return rttl

    parse = construct().parse



class RttlEncoder:
    # Note: although it may be specific to RTTL, the # or _ sign is
    # treated as part of the note itself and therefore produces a note
    # in the specified octave.  This has the effect of "wrapping" at
    # the octave boundary, so B#5 is the same as C5 rather than being
    # one semitone above B5.  Other notations may treat this different
    # but probably many where an explicit octave follows the note would
    # do it this way.  Note that if there's an implicit octave, it looks
    # wrong and maybe is handled wrong, if anyone actually uses B# in
    # RTTL source.  That is, with o=5, using B,B# would cause a drop in
    # pitch by 11 semitones rather than going up one semitone.
    # The same applies to C_ although the whole "_" convention is an
    # addition to the standard, and no doubt as fuzzily defined anyway.
    TONES = {
        'C':  0, 'B#': 0, 'H#': 0,
        'C#': 1, 'D_': 1,
        'D':  2,
        'D#': 3, 'E_': 3,
        'E':  4, 'F_': 4,
        'F':  5, 'E#': 5,
        'F#': 6, 'G_': 6,
        'G':  7,
        'G#': 8, 'A_': 8,
        'A':  9,
        'A#': 10, 'B_': 10, 'H_': 10,
        'B':  11, 'C_': 11, 'H': 11,
        # Note: apparently in Germany H may be a B natural, and B is a B flat
        # while in Estonia (?) and maybe other places it's just same as a B
        # Or it could be that the alternative H thing is a misinterpretation
        # of Germany's convention and nobody uses H that way at all.
        # Could add a flag to adjust which we use but for now it's "Estonian".
        # For Germany's approach we'd need to change 'B' to 10 and remove
        # the 'B_' entry I guess.
    }

    def __init__(self, debug=False, options=None):
        self.debug = debug
        self.volume = None
        self.options = options
        # Defaults, for if no settings given, from RTTL wiki page.
        # Numerous parsers out there insist on all three, in "dob" order,
        # as they usually insist on a title and fail if it's more than
        # 10 chars even when they have no reason to do so.
        self.defbeat = 4
        self.octave = 5
        self.bpm = 63
        self.log = log_print if self.debug else lambda *x: None


    def setting(self, setting):
        code, val = setting
        code = code.lower()
        val = int(val)
        if code == 'b':
            self.log('bpm', val)
            self.bpm = val
            self.music.set_tempo(self.bpm)

        elif code == 'd':
            self.log('defbeat', val)
            self.defbeat = val

        elif code == 'o':
            self.log('octave', val)
            self.octave = val

        elif code == 'v':
            self.log('volume', val)
            if self.volume is not None or val != MusicEncoder.DEFAULT_VOLUME:
                self.volume = val
                self.music.set_volume(self.volume)

        else:
            self.log(f'ignoring setting {code}={val}')


    def note(self, note):
        # print(f'note: {note!r}')
        dur = Fraction(1, int(note['d'] or self.defbeat))
        dotted = note['dot']
        if dotted:
            dur = Fraction(dur * 1.5)
        octave = int(note['o'] or self.octave)
        pitch = note['p'].upper()
        # some of the All_Songs_A2Z.txt pieces actually do crap like "4.#f5"!!
        if pitch.startswith('#') or pitch.startswith('_'):
            pitch = pitch[1:] + pitch[0]
        # self.log(f'--> {dur} {pitch} {octave if pitch != "p" else ""}')

        if pitch.upper() == 'P':
            self.music.add(Rest(duration=dur, src=note))
        else:
            # Caveat: most of what's written below came before I figured
            # out that generally in RTTL people interpret the signs as
            # being part of the note and within the octave, so B#5 is C5
            # rather than moving up to C6. Maybe that even fits better
            # with typical music theory understanding of things: the note
            # is B#, period, not B with a sharp adjustment.  I don't know
            # if that's how it's generally seen, but it at least appears
            # to be the common usage in RTTL source.  Some of what follows
            # may be obsolete now.

            # Warning: doing this this way may result in the wrong tone,
            # depending on how RTTL is supposed to interpret the octaves.
            # For example, if an octave starts with C, and we allow B#
            # to be B in the same octave then add one semitone, does
            # that make it C in the next octave, or does it wrap and become
            # C in the same octave?  And do we even know if RTTL sees
            # octaves as starting with C, or maybe it starts with A?
            # It's ambiguous in the docs.  I've looked for examples that
            # could prove/disprove this but so far haven't found an
            # unambiguous answer.
            # 1. https://codebender.cc/example/Tone/RTTTL#RTTTL.ino
            #    This implements code that treats "#" as merely increasing
            #    the count by one, so it does not turn B# into a C in the
            #    same octave, but in the next one.  (I think this is the
            #    most logical approach too.)
            # 2. https://github.com/adamonsoon/rtttl-parse/blob/master/src/index.js
            #    This does the same and seems clean so maybe trustworthy.
            # 3. https://arduinoplusplus.wordpress.com/2019/12/23/an-rtttl-parser-class/
            #    That page talks about the underscore convention but not
            #    how it's to be interpreted. It says:
            #    "Although not part of the original specification, some RTTTL
            #    strings also contain an extension for ‘flat’ notes which
            #    have defined equivalents – B_ or H_ (A#), C_ (B), D_ (C#),
            #    E_ (D#), F_ (E), G_ (F#), A_ (G#)." but that's still ambiguous.
            #    But their parser takes the opposite approach
            #    and interprets C_ as a B, *in the same octave*:
            #    https://github.com/MajicDesigns/MD_RTTTLParser/blob/master/src/MD_RTTTLParser.cpp
            #    Since this issue applies only at the transition between
            #    octaves, from B to C, and as long as nobody uses B# and
            #    we have to sort that out too, then it's probably okay
            #    to assume that C_4 is the same note as B4 rather than
            #    being one octave down.  In fact, it looks like as long
            #    as everyone implements RTTTL with the assumption that
            #    C is the first note, and octaves start at C, then
            #    C_ with an octave specified would have to be at the end
            #    of the octave, in which case the following code isn't
            #    appropriate.

            # To complicate matters still further, turns out that a lot
            # of existing ringtones apparently use "_" to indicate a sharp!
            # Possibly created by idiots, or maybe I'm missing something.
            # Anyway, added an option to handle it.
            if not self.options.get('flats'):
                if pitch.endswith('_'):
                    pitch = pitch[:-1] + '#'
            tone = self.TONES[pitch.upper()] + (octave + 1) * 12
            self.music.add(Note(duration=dur, tone=tone, src=note))


    def encode(self, text):
        '''Return Pavlok music encoding bytes for the given music in RTTL notation.'''
        src = RttlGrammar.parse(text)
        # Note: this assumes the name has been suppressed, so we get either
        # [(settings), (notes)] or just (notes)
        if src and src[0] == 'notes':
            settings = []
            notes = src[1]
        elif src[0][0] == 'settings' and src[1][0] == 'notes':
            settings = src[0][1]
            notes = src[1][1]
        else:
            self.log('parse error')
            settings = notes = []

        self.music = Music()

        if self.volume and self.volume != MusicEncoder.DEFAULT_VOLUME:
            self.music.set_volume(max(0, min(100, self.volume)))

        if settings:
            self.log(f'Settings: {settings}')
            for _, setting in settings:
                start, value, end = setting
                try:
                    self.setting(value)
                except Exception as ex:
                    span = (start, end)
                    raise ScriptError(f'error in music at {item!r}', span)

        if notes:
            for item in notes:
                # print('item', item)
                start = end = (0, 0)
                try:
                    name, item = item
                except ValueError:
                    name = ''

                try:
                    start, value, end = item
                except ValueError:
                    value = item

                try:
                    if name == 'setting':
                        self.setting(value)
                    elif name == 'note':
                        self.note(value)
                    else:
                        self.log('error')

                except Exception as ex:
                    span = (start, end)
                    raise ScriptError(f'error in music at {item!r}', span)

        return MusicEncoder(debug=self.debug).encode(self.music)


# Code to generate bytecode for Pavlok script language

def encode_int(val):
    if val < 128:
        return [val]
    elif val < 2**14 + 128:
        val -= 128
        return [0x80 | (val >> 8), val & 0xff]
    elif val < 2**20 + 16512:
        val -= 16512
        return [0xc0 | (val >> 16), (val >> 8) & 0xff, val & 0xff]
    else:
        raise ValueError(f'no support yet for ints >= {2**20 + 16512}')


class ScriptError(Exception):
    def __str__(self):
        try:
            s, e = self.args[1]
        except (TypeError, IndexError):
            loc = ''
        else:
            if s[0] == e[0]:
                if s[1] == e[1]:
                    loc = f' at {s[0]}:{s[1]}'
                else:
                    loc = f' at {s[0]}:{s[1]}-{e[1]}'
            else:
                loc = f' at {s[0]}:{s[1]}-{e[0]}:{e[1]}'

        return f'{self.args[0]}{loc}'



PBCODE_NOOP     = 0     # no-op
PBCODE_DEF_ITEM = 1     # define item
PBCODE_FUNC_REF = 2     # call function, one reference
PBCODE_FUNC_ARG = 3     # call function, one argument
PBCODE_FUNC_0   = 4     # call function, no arguments

PBFUNC_NONE     = 0     # no-op
PBFUNC_DELAY    = 1     # delay milliseconds
PBFUNC_VIBE     = 2     #
PBFUNC_BEEP     = 3     #
PBFUNC_ZAP      = 4     #
PBFUNC_MUSIC    = 5     #
PBFUNC_ACTION   = 6     #
PBFUNC_SCRIPT   = 7     #

OPCODE_MAP      = 0xF5
OPCODE_ARRAY1   = 0xF0


class Token:
    _count = 0

    def __init__(self, *args):
        start, data, end = args
        self.span = (start, end)
        self.data = data


    def __repr__(self):
        n = self.__class__.__name__
        s, e = self.span
        if s[0] == e[0]:
            span = f'{s[0]}:{s[1]}-{e[1]}'
        else:
            span = f'{s[0]}:{s[1]}-{e[0]}:{e[1]}'
        return f'<{n} ({span}) {self.data}>'


class Name(Token):
    def __init__(self, *args):
        super().__init__(*args)

class Value(Token):
    def __init__(self, *args):
        super().__init__(*args)
        self.name = ''  # in case someone tries
        self.value = self.data

class Pair(Token):
    def __init__(self, *args):
        super().__init__(*args)
        self.name = self.data[0]
        self.value = self.data[1]

class Command(Token):
    def __init__(self, *args):
        super().__init__(*args)
        self.name = self.data[0]
        self.args = self.data[1]
        # print(self, self.args)


def default_builder(f, *args):
    return [PBCODE_NOOP]


def build_none(f, args=None, options=None):
    return [PBCODE_FUNC_0, PBFUNC_NONE]


def build_int(f, args, options=None):
    if len(args) > 1:
        raise ScriptError('too many arguments', args[1].span)

    try:
        arg = args[0]
    except IndexError:
        raise ScriptError('numeric argument required')

    if not isinstance(arg, Value):
        raise ScriptError('expected simple value here', arg.span)

    val = arg.value

    if val.endswith('ms'):
        val = int(val.strip('sm '))
    elif val.endswith('s'):
        val = int(val.strip('sm ')) * 1000
    else:
        val = int(val.strip())
    return bytes([PBCODE_FUNC_ARG] + encode_int(f.code) + encode_int(val))


def build_map(f, args, options=None):
    # print(f.params)
    code = bytes([PBCODE_FUNC_ARG]) + bytes(encode_int(f.code))
    if not args:
        data = bytes([0xff])   # null, meaning use defaults
    else:
        mapdata = bytearray()
        for item in args:
            if not isinstance(item, Pair):
                raise ScriptError(f'missing value with {item.data!r}', item.span)

            name, value = item.data

            log(name, value)
            try:
                p = f.params[name.data]
            except KeyError:
                log(f'\tparam not recognized: {name}')
                raise ScriptError(f'unknown {f.name!r} param: {name.data!r}', name.span)

            log(f'\tparam: {p} = {value.data!r}')
            try:
                mapdata += bytes(encode_int(p.code)) + bytes(encode_int(int(value.data)))
            except ValueError as ex:
                raise ScriptError(f'bad value for {name.data!r}: {value.data} ({ex})', value.span)

        data = bytes([OPCODE_MAP]) + bytes(encode_int(len(mapdata))) + mapdata

    return code + data


def build_music(f, args, options=None):
    data = []
    for item in args:
        if not isinstance(item, Pair):
            raise ScriptError(f'missing value with {item.data!r}', item.span)

        name, value = item.data

        # log(f'build: {name} = {value}')
        try:
            param = f.params[name.data]

        except KeyError as ex:
            log(f'error: unrecognized musical notation {name.data}')
            continue

        try:
            music = value.data
            # print(f'music: {music!r}')
            # breakpoint()
            # Note: value.span is the full span of music in original source

            try:
                Encoder = globals()[f'{param.name.title()}Encoder']
            except KeyError:
                raise ScriptError(f'unsupported encoding {param.name}', name.span)

            encoder = Encoder(debug=debug, options=options)
            try:
                data = encoder.encode(music)

            except parsy.ParseError as ex:
                print(ex)
                # ex.lineinfo() => e.g. '0:6'
                # ex.index -> 6
                # ex.stream -> 'd=4:c,x '
                # ex.expected -> frozenset() with patterns
                line, col = map(int, ex.line_info().split(':'))
                # print(f'span {value.span}, line {line}, col {col}')
                # breakpoint()
                if line == 0:
                    # Need to account for length of line up to start of music
                    # Hack!
                    col += value.span[0][1] + len('[[')
                line += value.span[0][0]

                pos = (line, col)
                raise ScriptError('bad RTTL music notation', (pos, pos))

            except ScriptError as ex:
                if len(ex.args) < 2:
                    ex.args = (ex.args[0], item.span)
                raise

        except ValueError as ex:
            log(f'error: {ex}')
            # breakpoint()
            raise

        # self.log(name, value)
    return [PBCODE_FUNC_ARG] + encode_int(f.code) + [OPCODE_ARRAY1] + encode_int(len(data)) + data


class Param:
    def __init__(self, name, code, alias=[]):
        self.name = name
        self.aliases = [alias] if isinstance(alias, str) else alias
        self.code = code


    def __repr__(self):
        return f'<Param {self.name}={self.code}>'



class Function:
    _funcs = {}

    log = print if debug else lambda *x: None


    def __init__(self, name, alias=[], argcount=1, argtype=None, builder=None, code=None, params=None):
        self.name = name
        self.aliases = [alias] if isinstance(alias, str) else alias
        self.argcount = argcount
        self.code = code
        self.builder = builder or default_builder
        self.params = self.param_dict(params)


    def __repr__(self):
        return f'<Func {self.name}>'


    def __call__(self, _args, options={}):
        self.log(f'{self} calling {self.builder.__name__}({_args})')
        return self.builder(self, _args, options)


    def param_dict(cls, params):
        pdict = {}
        for p in params or []:
            pdict[p.name] = p
            for alias in p.aliases:
                pdict[alias] = p
        return pdict


    @classmethod
    def _define_funcs(cls, funclist):
        for f in funclist:
            cls._funcs[f.name] = f
            for alias in f.aliases:
                cls._funcs[alias] = f


Function._define_funcs([
    Function('nop', argcount=0, code=PBFUNC_NONE, builder=build_none, alias='noop'),
    Function('delay', argcount=1, argtype='int', code=PBFUNC_DELAY, builder=build_int,
        alias=['pause', 'sleep'],
        ),
    Function('vibe', argcount=1, argtype='map', code=PBFUNC_VIBE, builder=build_map,
        alias='v',  # note: would conflict with "v" as a shortcut for volt
        params=[
            Param('count', 1, alias='c'),
            Param('level', 2, alias='l'),
            Param('freq', 3, alias='f'),
            Param('t_on', 4, alias=['on', 'duration', 'dur']),
            Param('t_off', 5, alias='off'),
            Param('options', 6),
            ]),
    Function('beep', argcount=1, argtype='map', code=PBFUNC_BEEP, builder=build_map,
        alias=['b', 'tone'],
        params=[
            Param('count', 1, alias='c'),
            # Note: level/volume feature not supported yet
            Param('level', 2, alias=['l', 'vol', 'volume']),
            Param('freq', 3, alias=['f', 'frequency']),
            Param('t_on', 4, alias=['on', 'duration', 'dur']),
            Param('t_off', 5, alias=['off', 'gap']),
            Param('options', 6),
            ]),
    Function('zap', argcount=1, argtype='map', code=PBFUNC_ZAP, builder=build_map,
        # Per MS 2020-09-17 they're avoiding the term "zap" now (why?) and emphasizing
        # some others: snap, tap, stim, volt.  I noted possible conflict with double-tap
        # (which could support a single-tap in future) but left it in for now.
        alias=['z', 'snap', 'tap', 'stim', 'volt'],
        params=[
            Param('count', 1, alias='c'),
            Param('level', 2, alias='l'),
            Param('options', 6),
            ]),
    Function('music', argcount=1, argtype='bytes', code=PBFUNC_MUSIC, builder=build_music,
        alias='play',
        params=[
            # Param('volume', 0, alias='vol', builder=build_int),
            Param('rttl', 1, alias='rtttl'),
            Param('abc', 2),
            Param('mml', 3),
            ]),
    Function('action', argcount=1, argtype='int', code=PBFUNC_ACTION, builder=build_int,
        ),
    Function('script', argcount=1, argtype='int', code=PBFUNC_SCRIPT, builder=build_int,
        ),
])


# Make script grammar in a separate namespace.
# See RttlGrammar notes for explanation.
class ScriptGrammar:
    def construct():
        from parsy import (eof, regex, seq, string, string_from, whitespace, digit,
            char_from, generate)

        spaces = regex('[ \t]+').desc('req\'d space')
        opt_space = regex('[ \t]+').desc('opt space').optional()
        white = whitespace.desc('required white')
        opt_white = white.desc('opt white').optional()
        eol = string('\n')
        cmdsep = regex('[; \t\r\n]+').desc('cmd sep (; or eol)')
        # comment = regex(r'\s*#[^\n]*').desc('comment')

        datablock = string('[[') >> regex(r'(?ms).*?(?=\]\])') << string(']]')

        name = regex(r'[a-zA-Z_][a-zA-Z_0-9]*').mark().combine(Name)
        # TODO: move to something like this, which wasn't possible before
        # in a namespace until I figured out the construct() pattern.
        # This could then be enhanced to recognize the available function
        # names, and per-function parameters, and even pre-parse the
        # arguments and such so that we can generate errors as close to the
        # specific problem as possible, report errors as early as we can,
        # not report errors at the wrong place, and so on.
        # @generate('command')
        # def command():
        #     func = yield name.mark()
        #     yield opt_space
        #     if func[1][1] not in {'zap'}:
        #         breakpoint()
        #     return [func[1]]
        command = (name << opt_space).map(lambda x: (x, []))    # add dummy args

        argsep = regex(r'\s*,\s*')
        # will need to strip spaces later
        value = (datablock | regex('[^,;\n]+')).desc('value').mark().combine(Value)
        colon = string(':')
        pair = seq(name << spaces, value).mark().combine(Pair)
        blankline = string('')
        args = ((opt_space >> (pair | value) << opt_space).sep_by(argsep) << opt_space) # .mark().combine(Args)
        command_arg = seq(name << opt_space << colon << opt_white, args)
        script = cmdsep.optional() >> (command_arg | command).mark().combine(Command).sep_by(cmdsep) << cmdsep.optional()

        return script

    parse = construct().parse


class ScriptParser:
    def __init__(self, debug=False):
        self.debug = debug
        self.log = log_print if debug else lambda *x: None


    def generate(self, cmd, **options):
    
        #self.log(repr(cmd))
        #print(repr(cmd))

        # breakpoint()
        name = cmd.data[0]
        args = cmd.args
        try:
            f = Function._funcs[name.data]
        except KeyError as ex:
            # self.log(f'invalid command {name}')
            raise ScriptError(f'unknown function {name.data!r}', name.span) from None
        else:
            try:
                return f(args, options)
            except ScriptError as ex:
                try:
                    span = ex.args[1]
                    if not span: raise IndexError
                except IndexError:
                    ex.args = (ex.args[0], name.span)

                raise

            # except ValueError, TypeError):
            #     raise ScriptError(f'bad data for {name.data!r} ({text})', cmd.span)

            except Exception as ex:
                text = f'{ex.__class__.__name__}: {ex}'
                tb.print_tb(ex.__traceback__)
                raise


    def parse(self, source, **kwargs):
        data = []
        for cmd in ScriptGrammar.parse(source):
            if not isinstance(cmd, Command):
                continue

            # self.log(cmd)
            # self.log(f'\tname: {cmd.name}')
            # self.log(f'\targs: {cmd.args}')

            try:
                bcodes = self.generate(cmd, flats=kwargs.get('flats'))
                if bcodes:
                    # self.log(f'\t-> bytecode: {bcodes}')
                    data.extend(bytes(bcodes))

            except (TypeError, ValueError) as ex:
                try:
                    tb.print_tb(ex.__traceback__)
                except Exception as ex1:
                    breakpoint()
                    pass
                raise ScriptError(f'bad data from {cmd.name!r}', cmd.span)
                # self.log(f'error: {cmd}')
                # breakpoint()
                # raise
        return bytes(data)


    _seq_count = itertools.count(1)

    def payload(self, bytecode, seq=None, ver=0):
        crc = calc_crc16(bytecode, 0xffff)

        if seq == 'time':
            stimid = int(time.time())
        elif seq == 'random':
            import random
            stimid = random.randint(0, 254)
        elif seq == 'count':
            stimid = next(self._seq_count)
        else:
            stimid = None

        stimid = ((stimid % 255) + 1) if stimid is not None else 0

        if ver == 0:
            packet = struct.pack('<HBB', len(bytecode) + 2, stimid & 0xff, 0) \
                + bytecode + struct.pack('<H', crc)
        else:
            packet = struct.pack('<HBBH', len(bytecode), stimid & 0xff, 0, crc) + bytecode
        padded = packet + b'\0' * ((4 - len(packet)) % 4)
        self.log(f'padded: {padded.hex()}')
        payload = b'PB' + z85.encode(padded)

        return dict(stimid=stimid, payload=payload.decode())


    def api(self, **req):
        res = {
            'error': '',
            }

        try:
            source = req.get('source', '').rstrip()
            res['source'] = source

            data = self.parse(source, flats=req.get('flats'))
            res['bytecode'] = '\n'.join(textwrap.wrap(re.sub(r'(..)', r'\1 ', data.hex()), 3*16))
            if req.get('pb'):
                res.update(self.payload(data, seq=req.get('seq', ''), ver=req.get('ver', 1)))

        except parsy.ParseError as ex:
            lines = source.splitlines()
            line, col = map(int, ex.line_info().split(':'))
            # breakpoint()
            res.update({
                'error': str(ex),
                'line': lines[line] if line < len(lines) else '[end of script]',
                'span': ex.args[1],
                'marker': ' ' * col + '^',
            })

        except ScriptError as ex:
            lines = source.splitlines()
            line, col = ex.args[1][0] # first pair of span, may be same as second
            res.update({
                'error': str(ex),
                'line': lines[line] if line < len(lines) else '[end of script]',
                'span': ex.args[1],
                'marker': ' ' * col + '^',
            })

        except Exception as ex:
            res.update({
                'error': repr(ex),
                'tb': '\n'.join(tb.format_tb(ex.__traceback__)),
            })

        res['okay'] = not res.get('error')

        return res


RTTL_TEST = '''Name of tune:d=5,o = 6:
a.,a#.,8a#.,8a#4.,8a#,d,e,2f,8f#.,8f.,d.6,8c#6.,a#,2a,8g,8a,a#,a#,f,d,a#4, 8f.,16d,a#4,d,f,2a#,
    8d.6,16c6,a#,d,e,2f,8f,8f,d.6,8c6,a#,d = 2,o=5,2a,8g,8a,a#,a#,  f , d,a#4,8d6,8d6,d6,

    d#6,f6,2f6,8d#6,8d6,c6,d6,d#6,2d#6,d#6,d.6,8c6,a#,2a,8g,8a,a#,d,e,2f,f,
    a#,a#,8a#,8a,g,g,g,c6,8d#6,8d6,8c6,8a#,a#,a,8f,8f,a#.,8c6,8d6,8d#6,2f6,
    8a#,8c6,d.6,8d#6,c6,1a#
'''

SCRIPT_TEST = '''
    beep: freq 40, count 3
    delay: 500
    vibe: level 100
    zap: level 70
    music: rttl [[ Simpsons:d=4,o=5,b=160:32p,c.6,e6,f#6,8a6,g.6,e6,c6,8a,8f#,8f#,8f#,2g ]]
'''

def run_tests():
    # print(RttlGrammar.parse('d=2x,c'))
    ScriptParser().parse(SCRIPT_TEST)
    sys.exit(0)


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--debug', action='store_true')
    parser.add_argument('-m', '--music')
    parser.add_argument('-v', '--volume', type=int, default=None)
    parser.add_argument('--test', action='store_true')
    parser.add_argument('--api', action='store_true')
    parser.add_argument('--seq')
    parser.add_argument('--ver', type=int, default=1)
    parser.add_argument('script', nargs='?')

    args = parser.parse_args()

    if args.test:
        run_tests()     # does not return

    if args.music:
        source = f'music: rttl [[ {args.music} ]]'
    elif args.script:
        source = args.script
    else:
        print('Reading from stdin...')
        source = sys.stdin.read()

    p = ScriptParser(debug=args.debug)

    if args.api:
        import json
        req = json.loads(args.script)
        req['ver'] = args.ver
        result = p.api(**req)
        print(result)
    else:
        result = p.api(source=source, pb=True, ver=args.ver)

        if result['okay']:
            bytecode = result['bytecode']
            # breakpoint()
            text = bytecode # as_hex_escapes(bytecode)
            print(f'bytecode: "{text}" (len={len(bytecode)})')

            raw = bytes(b'PB' + z85.decode(result['payload'][2:]))
            print(f'raw hex: {raw.hex()} (len={len(raw)})')

            payload = result['payload']
            print(f'\nZ85: "\\n{payload}"\n')

            with open('z85.out', 'w') as f:
                f.write(r'\n' + payload.strip())

        else:
            print('Error:', result['error'])
            try:
                print(result['line'])
                print(result['marker'])
            except KeyError:
                pass

            try:
                print()
                print(result['tb'])
            except KeyError:
                pass


# EOF