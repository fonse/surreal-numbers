import Test.HUnit
import Surreals

_0 = N [] []
_1 = N [_0] []
_2 = N [_1] []
_1'2 = N [_0] [_1]
_m1 = N [] [_0]
_m2 = N [] [_m1]
_m1'2 = N [_m1] [_0]

testsEquality = "Equality" ~: TestList
  [    _0 == _0    ~?    "0 = 0"
  ,  _1'2 == _1'2  ~?  "1/2 = 1/2"
  ,    _1 == _1    ~?    "1 = 1"
  ,    _2 == _2    ~?    "2 = 2"
  , _m1'2 == _m1'2 ~? "-1/2 = -1/2"
  ,   _m1 == _m1   ~?   "-1 = -1"
  ,   _m2 == _m2   ~?   "-2 = -2" ]

testsOrder = "Order" ~: TestList
  [   _m2 < _m1   ~?   "-2 < -1"
  ,   _m1 < _m1'2 ~?   "-1 < -1/2"
  , _m1'2 < _0    ~? "-1/2 < 0"
  ,    _0 < _1'2  ~?    "0 < 1/2"
  ,  _1'2 < _1    ~?  "1/2 < 1"
  ,    _1 < _2    ~?    "1 < 2"
  ,   _m2 < _0    ~?   "-2 < 0"
  ,    _0 < _2    ~?    "0 < 2"
  ,   _m2 < _2    ~?   "-2 < 2"
  , _m1'2 < _1'2  ~?  "-1/2 < 1/2" ]

main :: IO Counts
main = runTestTT $ TestList [testsEquality, testsOrder]
