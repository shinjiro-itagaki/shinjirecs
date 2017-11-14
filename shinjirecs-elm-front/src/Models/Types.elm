module Models.Types exposing (Entity,map9,map10,map12,map15,map16,Encoder,fromTimeToDateDecoder)
import Json.Decode as D
import Json.Encode exposing (Value)
import Date exposing (fromTime)

type alias Entity a =
    { id  : Int
    , val : a
    }

type alias Encoder a = (a -> Value)
    
map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
     -> D.Decoder a
     -> D.Decoder b
     -> D.Decoder c
     -> D.Decoder d
     -> D.Decoder e
     -> D.Decoder f
     -> D.Decoder g
     -> D.Decoder h
     -> D.Decoder i
     -> D.Decoder value

map9 f9 d1 d2 d3 d4 d5 d6 d7 d8 d9 =
    D.map2
        (\(x1,x2,x3,x4,x5) (x6,x7,x8,x9) -> f9 x1 x2 x3 x4 x5 x6 x7 x8 x9)
        (D.map5
             (\a1 a2 a3 a4 a5 -> (a1,a2,a3,a4,a5))
             d1 d2 d3 d4 d5)
        (D.map4
             (\a6 a7 a8 a9 -> (a6,a7,a8,a9))
             d6 d7 d8 d9)

    
map10 : (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> value)
     -> D.Decoder a1
     -> D.Decoder a2
     -> D.Decoder a3
     -> D.Decoder a4
     -> D.Decoder a5
     -> D.Decoder a6
     -> D.Decoder a7
     -> D.Decoder a8
     -> D.Decoder a9
     -> D.Decoder a10
     -> D.Decoder value

map10 f10 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 =
    D.map2
        (\(x1,x2,x3,x4,x5) (x6,x7,x8,x9,x10) -> f10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
        (D.map5
             (\a1 a2 a3 a4 a5 -> (a1,a2,a3,a4,a5))
             d1 d2 d3 d4 d5)
        (D.map5
             (\a6 a7 a8 a9 a10 -> (a6,a7,a8,a9,a10))
             d6 d7 d8 d9 d10)
            
map12 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> value)
     -> D.Decoder a
     -> D.Decoder b
     -> D.Decoder c
     -> D.Decoder d
     -> D.Decoder e
     -> D.Decoder f
     -> D.Decoder g
     -> D.Decoder h
     -> D.Decoder i
     -> D.Decoder j
     -> D.Decoder k
     -> D.Decoder l
     -> D.Decoder value

map12 f12 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 =
    D.map2
        (\(x1,x2,x3,x4,x5,x6) (x7,x8,x9,x10,x11,x12) -> f12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
        (D.map6
             (\a1 a2 a3 a4 a5 a6 -> (a1,a2,a3,a4,a5,a6))
             d1 d2 d3 d4 d5 d6)
        (D.map6
             (\a7 a8 a9 a10 a11 a12 -> (a7,a8,a9,a10,a11,a12))
             d7 d8 d9 d10 d11 d12)            

map15 : (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13 -> a14 -> a15 -> value)
     -> D.Decoder a1
     -> D.Decoder a2
     -> D.Decoder a3
     -> D.Decoder a4
     -> D.Decoder a5
     -> D.Decoder a6
     -> D.Decoder a7
     -> D.Decoder a8
     -> D.Decoder a9
     -> D.Decoder a10
     -> D.Decoder a11
     -> D.Decoder a12
     -> D.Decoder a13
     -> D.Decoder a14
     -> D.Decoder a15
     -> D.Decoder value

map15 f15 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 =
    D.map4
        (\(x1,x2,x3,x4) (x5,x6,x7,x8) (x9,x10,x11,x12) (x13,x14,x15) -> f15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)
        (D.map4
             (\a1 a2 a3 a4 -> (a1,a2,a3,a4))
             d1 d2 d3 d4)
        (D.map4
             (\a5 a6 a7 a8 -> (a5,a6,a7,a8))
             d5 d6 d7 d8)
        (D.map4
             (\a9 a10 a11 a12 -> (a9,a10,a11,a12))
             d9 d10 d11 d12)
        (D.map3
             (\a13 a14 a15 -> (a13,a14,a15))
             d13 d14 d15)

            
map16 : (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13 -> a14 -> a15 -> a16 -> value)
     -> D.Decoder a1
     -> D.Decoder a2
     -> D.Decoder a3
     -> D.Decoder a4
     -> D.Decoder a5
     -> D.Decoder a6
     -> D.Decoder a7
     -> D.Decoder a8
     -> D.Decoder a9
     -> D.Decoder a10
     -> D.Decoder a11
     -> D.Decoder a12
     -> D.Decoder a13
     -> D.Decoder a14
     -> D.Decoder a15
     -> D.Decoder a16
     -> D.Decoder value

map16 f16 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 =
    D.map4
        (\(x1,x2,x3,x4) (x5,x6,x7,x8) (x9,x10,x11,x12) (x13,x14,x15,x16) -> f16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16)
        (D.map4
             (\a1 a2 a3 a4 -> (a1,a2,a3,a4))
             d1 d2 d3 d4)
        (D.map4
             (\a5 a6 a7 a8 -> (a5,a6,a7,a8))
             d5 d6 d7 d8)
        (D.map4
             (\a9 a10 a11 a12 -> (a9,a10,a11,a12))
             d9 d10 d11 d12)
        (D.map4
             (\a13 a14 a15 a16 -> (a13,a14,a15,a16))
             d13 d14 d15 d16)

fromTimeToDateDecoder = D.float |> D.andThen (D.succeed << fromTime)
