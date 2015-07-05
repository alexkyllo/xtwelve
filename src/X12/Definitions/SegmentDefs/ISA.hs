{-# LANGUAGE OverloadedStrings #-}
-- | ISA Segment Definition

module X12.Definitions.SegmentDefs.ISA where
import X12.Definitions.Requirement
import X12.Definitions.RepeatCount
import X12.Definitions.SegmentDefs
import X12.Definitions.ElementDefs
import X12.Definitions.ElementUse

isa = SegmentDef { segmentId = "ISA"
                 , elementUses = [ ElementUse { elementUseDef = i01
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i02
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i03
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i04
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i05
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i06
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i05
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i07
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i08
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i09
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i10
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i01
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i11
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i12
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i13
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i14
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 , ElementUse { elementUseDef = i15
                                              , elementReq = Mandatory
                                              , elementRepeatCount = Bounded 1
                                              }
                                 ]
                 }
