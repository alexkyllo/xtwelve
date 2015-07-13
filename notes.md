## Tokenizing / Parsing process

1. Tokenizer reads ISA segment to detect separators, constructs a SegmentToken consisting of SimpleElementTokens representing the ISA
2. Tokenizer reads subsequent segments up to and including an IEA segment
3. Tokenizer returns a [SegmentToken] (list of SegmentTokens) representing the interchange
4. Parser constructs an InterchangeVal
5. Parser takes the ISA SegmentToken and looks up the InterchangeDef's SegmentUses by the ISA12 version number.
6. Parser takes the Segment ID (first ElementToken in the SegmentToken) and looks up the SegmentDef's ElementUses. It uses the ElementUses to parse the ElementToken into an ElementVal.
