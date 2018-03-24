#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
" Brittany Lei  bclei@ucsc.edu  asg3 "

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) &
         (char asInteger <= other char asInteger))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor value: char value: self isleaf.
   ]

   isleaf [
      ^true.
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor [
      left postorder: visitor.
      right postorder: visitor.
      visitor value: char value: self isleaf.
   ]
   isleaf [
      ^false.
   ]

   left  [ ^ left]
   right [ ^ right]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]


nl := Character nl.
exit := 0.

args := Smalltalk arguments.
"to check if 3 arguments"
thrarg := args size = 3.
(args size > 0) ifTrue: [
  opt := args at: 1.
  (args size > 1) ifTrue: [
    file1 := args at: 2.
  ].
  thrarg ifTrue: [file2 := args at: 3.].

]
ifFalse: [
  stdout << 'Error: incorrect number of arguments'
         << nl.
  exit := 1.
].

optc := opt includes: $c.
optd := opt includes: $d.
optt := opt includes: $t.
optu := opt includes: $u.

(optc | optd | optt | optu) ifFalse: [
  stderr << 'Error: options word not supplied' << nl.
  exit := 1.
].

(args size > 1) ifTrue: [
  [
    (optu) ifFalse: [
      "read input file"
      rdfile := FileStream open: file1
                           mode: FileStream read.

      freqtable := ZeroArray new: 257.
      encodetable := ZeroArray new: 257.

      "count character frequency"
      [rdfile atEnd not] whileTrue: [
        |ordChar|
        ordChar := rdfile next asInteger.
        freqtable incr: ordChar.
      ].
      rdfile close.
      "add EOF entry"
      freqtable at: 256 put: 1.

      priority := SortedCollection new.
      "iterate over freqtab"
      freqtable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
          "create leaf for each nz entry and insert into queue"
          leafy := Leaf new: key asCharacter count: value.
          priority add: leafy.
        ]
      ].

      (optd) ifTrue: [
        stdout << nl << 'Leaves: '.
        priority do: [:item |
           stdout << item << nl.
        ].
        stdout << nl.
      ].
      priority do: [:item |
         nothing := item.
      ].

      [priority size > 1] whileTrue: [
        |first second tocount treeb|
        "remove 2 smallest from queue"
        first := priority removeFirst.
        second := priority removeFirst.
        tocount := first count + second count.
        "make new tree with smaller left, larger right"
        treeb := Tree new: first char count: tocount
                left: first right: second.
        priority add: treeb
      ].
      (optd) ifTrue: [
        stdout << 'Final Tree: '.
        priority do: [:item |
           stdout << item << nl << nl.
        ].
      ].

      thrarg ifTrue: [
        wrtfile := FileStream open: file2
                              mode: FileStream write.
      ].

      treeb := priority removeFirst.
      "for each nz, construct encoding string w dfs"
      treeb inorder: [:char :string |
        encodetable at: (char asInteger) put: string.
      ] prefix: ''.

      (optt) ifTrue: [
        count := 0.
        (optd) ifTrue: [
          stdout << 'Encoding Table' << nl .
        ].
        encodetable keysAndValuesDo: [:key :value |
          (value notNil) ifTrue: [
            frq := freqtable at: key.
            "format %3c%5d%s"
            cspace := ''.
            fspace := '    '.
            key > 32 ifTrue: [
              cspace := ' '.
            ].
            key > 126 ifTrue: [
              cspace := ''.
            ].

            frq > 9 ifTrue: [
              fspace := '   '.
            ].
            frq > 99 ifTrue: [
              fspace := '  '.
            ].
            frq > 999 ifTrue: [
              fspace := ' '.
            ].
            frq > 9999 ifTrue: [
              fspace := ''.
            ].

            key = 256 ifTrue: [
              show := 'EOF'.
              cspace := ''.
            ]
            ifFalse: [
             show := key asCharacter visible.
             key < 16 ifTrue: [
               cat := 'x0'
             ]
             ifFalse: [cat := 'x'].
              ((key < 33) | ((key > 126) & (key < 256))) ifTrue: [
              show := show copyReplaceAll: '16r' with: cat.
              ].
              key > 32 ifTrue: [
                show := show copyReplaceAll: '$' with: ''.
              ].
            ].

            thrarg ifTrue: [
              frq := freqtable at: key.
              fspace := '    '.
              frq > 9 ifTrue: [
                fspace := '   '.
              ].
              frq > 99 ifTrue: [
                fspace := '  '.
              ].
              frq > 999 ifTrue: [
                fspace := ' '.
              ].
              frq > 9999 ifTrue: [
                fspace := ''.
              ].
              wrtfile << cspace << show << cspace << ' '
                      << fspace << frq << ' ' << value
                      << nl.
            ]
            ifFalse: [
              stdout << cspace << show << cspace << ' '
                     << fspace << frq << ' ' << value
                     << nl.
            ].
          ].
        ].
      ].

      "option -c, compress"
      (optc) ifTrue: [
        |bcount|
          (optd) ifTrue: [
            stdout << 'Compressing' << nl << nl.
          ].
          bcount := 0.
          outBytes := OutBits new: wrtfile.
          "write encoding table in big-endian"
          treeb postorder: [:char :isleaf|
          "for interior node, write 1 bit"
          isleaf ifFalse: [
            thrarg ifTrue: [
              bit := 1 asInteger bitAt: 1.
              outBytes writeBit: bit.
            ]
            ifFalse: [stdout << 1].
            bcount := bcount + 1.
          ]
          "for leaf, write 0 bit, then 8 bits of corres byte"
          ifTrue: [
            thrarg ifTrue: [
              outBytes writeBit: 0.
            ]
            ifFalse: [stdout << 0.].
            bcount := bcount + 1.

            8 to: 1 by: -1 do: [:ind |
              bit := char asInteger bitAt: ind.
              thrarg ifTrue: [
                outBytes writeBit: bit.
              ]
              ifFalse: [stdout << bit.].
              bcount := bcount + 1.
            ].
            intchar := char asInteger.
            "if byte = 1, write out bit 8 (1)"
            intchar = 256 ifTrue: [
              thrarg ifTrue: [
                outBytes writeBit: 1.
              ]
              ifFalse: [stdout << 1.].
              bcount := bcount + 1.
            ].
            "if byte = 0, write out bit 8 (0)"
            intchar = 0 ifTrue: [
              thrarg ifTrue: [
                outBytes writeBit: 0.
              ]
              ifFalse: [stdout << 0.].
              bcount := bcount + 1.
            ].
          ].
        ].
        "encoding tree complete!"
        thrarg ifTrue: [
          outBytes writeBit: 1.
        ]
        ifFalse: [stdout << 1.].
        bcount := bcount + 1.

        "reopen input file to write encoded bytes"
        rdfile := FileStream open: file1
                             mode: FileStream read.
        "bitrfile := BitStream new: rdfile."
        [rdfile atEnd not] whileTrue: [
          |ordChar|
          ordChar := rdfile next asInteger.
          ec := encodetable at: ordChar.
          "write encoding to wrtfile"
          "iterate through bits"
          1 to: ec size by: 1 do: [:ind |
            ebit := (ec at: ind) asInteger.
            thrarg ifTrue: [
              outBytes writeBit: ebit.
            ]
            ifFalse: [stdout << (ec at: ind).].
            bcount := bcount + 1.

          ].
        ].
        "write encoding for EOF"
        ec := encodetable at: 256.
        1 to: ec size by: 1 do: [:ind |
          ebit := (ec at: ind) asInteger.
          thrarg ifTrue: [
            outBytes writeBit: ebit.
          ]
          ifFalse: [stdout << (ec at: ind).].
          bcount := bcount + 1.
        ].
        "pad with zeros if total bits not divis by 8"
        thrarg ifTrue: [
          0 to: (7 - (bcount \\ 8)) do: [:skip |
            outBytes writeBit: 0.
          ].
        ].

      rdfile close.
      thrarg ifTrue: [wrtfile close.].
      ].
    ]
    "option is -u (decompress)"
    ifTrue: [
      (optd) ifTrue: [
        stdout << 'Decompressing' << nl.
      ].

      "reopen read and write files"
      rdfile := FileStream open: file1
                          mode: FileStream read.
      thrarg ifTrue: [
        wrtfile := FileStream open: file2
                              mode: FileStream write.
      ].
      outBytes := OutBits new: wrtfile.
      inBytes := BitStream new: rdfile.

      stack := OrderedCollection new.
      "reached end of file?"
      fend := false.
      "tree completed?"
      trcomp := false.

      [(inBytes atEnd not) & (fend not)] whileTrue: [
        byt := 0.
        "rebuild encoding tree"
        trcomp ifFalse: [
        nxbit := inBytes nextBit.
          "if 0, is leaf"
          nxbit = 0 ifTrue: [
            8 to: 1 by: -1 do: [ :ind |
              bit := inBytes nextBit.
              byt := byt bitAt: ind put: bit.
            ].
            "if byte is 0, either 0 or EOF"
            byt = 0 ifTrue: [
              exbit := inBytes nextBit.
              exbit = 1 ifTrue: [
                "EOF"
                byt := byt + 256.
              ].
            ].
            stack add: (Leaf new: byt count: 0).
          ].
          "if 1, is interior node"
          nxbit = 1 ifTrue: [
            (stack size > 1) ifTrue: [
              |first second treeb|
              second := stack removeLast.
              first := stack removeLast.
              "build new tree, 1-subtree left, 0-subtree right"
              treeb := Tree new: 0 count: 0
                       left: first right: second.
              stack add: treeb.
            ]
            ifFalse: [
              trcomp := true.
              (optd) ifTrue: [
                stack do: [:item |
                  stdout << nl << 'Tree: ' << item << nl << nl.
                ].
              ].
              "stack empty, tree is rebuilt"
              decodetree := stack removeFirst.
              "pointer at beginning of tree for decoding"
              ptr := decodetree.
            ].
          ].
        ]
        ifTrue: [
        nxbit := inBytes nextBit.
          thrarg ifTrue: [
            ptr isleaf ifTrue: [
              (ptr char) = 256 ifTrue: [
                fend = true.
              ]
              ifFalse: [
                "write out corresponding byte"
                8 to: 1 by: -1 do: [:ind |
                  bit := ptr char bitAt: ind.
                  outBytes writeBit: bit.
                ].
                "reset pointer to beginning of tree"
                ptr := decodetree.
              ].
            ]
            "moves down tree to next node"
            ifFalse: [
              nxbit = 0 ifTrue: [
                ptr := ptr left.
              ]
              ifFalse: [
                ptr := ptr right.
              ].
              fend ifFalse: [
                ptr isleaf ifTrue: [
                  (ptr char) = 256 ifTrue: [
                    "end of file if char EOF"
                    fend = true.
                  ]
                  ifFalse: [
                    "write out corresponding byte"
                    8 to: 1 by: -1 do: [:ind |
                      bit := ptr char bitAt: ind.
                      outBytes writeBit: bit.
                    ].
                    "reset pointer to beginning of tree"
                    ptr := decodetree.
                  ].
                ].
              ].
            ].
          ]
          ifFalse: [
            ptr isleaf ifTrue: [
              (ptr char) = 256 ifTrue: [
                fend = true.
              ]
              ifFalse: [
                "write out corresponding byte"
                stdout nextPut: (Character value: (ptr char)).
                "reset pointer to beginning of tree"
                ptr := decodetree.
              ].
            ]
            "moves down tree to next node"
            ifFalse: [
              nxbit = 0 ifTrue: [
                ptr := ptr left.
              ]
              ifFalse: [
                ptr := ptr right.
              ].
              fend ifFalse: [
                ptr isleaf ifTrue: [
                  (ptr char) = 256 ifTrue: [
                    "end of file if char EOF"
                    fend = true.
                  ]
                  ifFalse: [
                    "write out corresponding byte"
                    stdout nextPut: (Character value: (ptr char)).
                    "reset pointer to beginning of tree"
                    ptr := decodetree.
                  ].
                ].
              ].
            ].
          ].
        ].
      ].
      rdfile close.
      thrarg ifTrue: [
        wrtfile close.
      ]
    ].

    "Program Finished"
    (optd) ifTrue: [
      stdout << 'Exit Status: ' << exit << nl.
      stdout << 'Program Finished.' << nl.
    ].
  ] on: SystemExceptions.FileError do: [:signal |
      |errno|
      errno := File errno.
      stdout flush.
      stderr << execname << ': ' << filename << ': '
             << signal messageText << '; invalid file' << nl.
      stderr flush.
      exit := 1.
    ]
]
ifFalse: [
  stderr << 'Error: invalid # of arguments' << nl.
  exit := 1.
].
exit = 1 ifTrue: [
  stderr << 'Exit Status: ' << exit << nl.
].
