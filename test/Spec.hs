import Test.HUnit
import Surreals

_0 = Surreal [] []
_1 = Surreal [_0] []
_2 = Surreal [_1] []
_1'2 = Surreal [_0] [_1]
_m1 = Surreal [] [_0]
_m2 = Surreal [] [_m1]
_m1'2 = Surreal [_m1] [_0]

testsEquality = "Equality with the same form" ~: TestList
  [    _0 == _0    ~?    "0 = 0"
  ,  _1'2 == _1'2  ~?  "1/2 = 1/2"
  ,    _1 == _1    ~?    "1 = 1"
  ,    _2 == _2    ~?    "2 = 2"
  , _m1'2 == _m1'2 ~? "-1/2 = -1/2"
  ,   _m1 == _m1   ~?   "-1 = -1"
  ,   _m2 == _m2   ~?   "-2 = -2"
  ]

testsEquality2 = "Equality with different forms" ~: TestList
  [ Surreal [_m1] [_1] == _0 ~? "{-1|1} = 0"
  , Surreal [_m2] [_1'2] == _0 ~? "{-2|1/2} = 0"
  , Surreal [_m2, _m1] [_0, _1, _2] == _m1'2 ~? "{-2,-1|0,1,2} = -1/2"
  ]

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
  , _m1'2 < _1'2  ~?  "-1/2 < 1/2"
  ]

testsAddition = "Addition" ~: TestList
  [   _0 + _2   == _2        ~?     "0 + 2 = 2"
  ,   _1 + _m1  == _0        ~?    "1 + -1 = 0"
  , _1'2 + _1'2 == _1        ~? "1/2 + 1/2 = 1"
  ,   _1 + _1'2 == _2 - _1'2 ~?   "1 + 1/2 = 2 - 1/2"
  ]

testsMultiplication = "Multiplication" ~: TestList
  [    _0 * _2 == _0  ~?   "0 * 2 = 0"
  ,   _m1 * _2 == _m2 ~?  "-1 * 2 = -2"
  ,  _1'2 * _2 == _1  ~? "1/2 * 2 = 1"
  ]

testsArithmetic = "Basic arithmetic" ~: TestList
  [ _m2 + _1'2 * _2 == _1 - _2 ~?   "-2 + 1/2 * 2 = 1 - 2"
  , _2 * (_2 + _1) == _2 * _2 + _2 * _1 ~?  "2 * (2+1) = 2*2 + 2*1"
  ]

main :: IO Counts
main = runTestTT $ TestList
  [ testsEquality
  , testsEquality2
  , testsOrder
  , testsAddition
  , testsMultiplication
  , testsArithmetic
  ]
