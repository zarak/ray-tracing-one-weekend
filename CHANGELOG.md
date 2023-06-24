# Revision history for ray-tracing-one-weekend

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 0.1.0.1 
* Optimizations and profiling

Parameters
```haskell
samplesPerPixel :: Int
samplesPerPixel = 100

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
imageWidth = 40
```

Flags
```
cabal run ray-tracing-one-weekend --ghc-options="-O2 -fprof-auto" --enable-profiling -- +RTS -P
```

### Inline pragma for vector subtraction
```haskell
  v - w = Vec3 (v.x - w.x) (v.y - w.y) (v.z - w.z)
  {-# INLINE (-) #-}
```

Before
```
	Sat Jun 24 14:52 2023 Time and Allocation Profiling Report  (Final)

	   ray-tracing-one-weekend +RTS -P -RTS

	total time  =       50.54 secs   (50542 ticks @ 1000 us, 1 processor)
	total alloc = 58,108,777,712 bytes  (excludes profiling overheads)

COST CENTRE        MODULE    SRC                              %time %alloc  ticks     bytes

hit                Sphere    src/Sphere.hs:(17,3)-(34,103)     32.8    0.1  16558  57025848
-                  Vec3      src/Vec3.hs:27:3-50               16.0    9.7   8091 5634053728
hit.oc             Sphere    src/Sphere.hs:21:9-39             11.3   29.0   5731 16847195904
hit                Sphere    src/Sphere.hs:(51,3)-(64,54)       8.6   21.4   4354 12421111768
findNearestRoot    Sphere    src/Sphere.hs:(39,1)-(46,47)       7.9    0.0   3994   6912240
lengthSquared      Vec3      src/Vec3.hs:57:1-51                5.1    9.7   2564 5628614384
hit.c              Sphere    src/Sphere.hs:24:9-60              4.4    9.7   2249 5615731968
hit.a              Sphere    src/Sphere.hs:22:9-43              2.4    3.2   1234 1871910656
hit.halfB          Sphere    src/Sphere.hs:23:9-36              2.1    3.2   1051 1871910656
randomInUnitSphere RtWeekend src/RtWeekend.hs:(46,1)-(50,15)    1.5    2.9    750 1694182016
hit.sqrtd          Sphere    src/Sphere.hs:26:9-33              1.4    0.0    699         0
randomDoubleR      RtWeekend src/RtWeekend.hs:(34,1)-(38,23)    1.3    1.6    665 956636776
lambertian         Material  src/Material.hs:(12,1)-(22,19)     1.2    2.0    595 1151823304
mkWorld            MyLib     src/MyLib.hs:118:1-34              1.0    1.8    490 1022208000
randomScene        MyLib     src/MyLib.hs:(121,1)-(158,65)      0.2    2.3     85 1351965080
```

After 
```
	Sat Jun 24 14:55 2023 Time and Allocation Profiling Report  (Final)

	   ray-tracing-one-weekend +RTS -P -RTS

	total time  =       52.58 secs   (52580 ticks @ 1000 us, 1 processor)
	total alloc = 37,496,581,696 bytes  (excludes profiling overheads)

COST CENTRE        MODULE    SRC                              %time %alloc  ticks     bytes

hit                Sphere    src/Sphere.hs:(17,3)-(34,103)     52.0    0.2  27360  57025864
-                  Vec3      src/Vec3.hs:27:3-50                9.0   15.0   4714 5634053728
findNearestRoot    Sphere    src/Sphere.hs:(39,1)-(46,47)       7.3    0.0   3858   6912240
hit.oc             Sphere    src/Sphere.hs:21:9-39              7.1   39.9   3753 14975285248
lengthSquared      Vec3      src/Vec3.hs:58:1-51                5.0    5.0   2639 1884793072
hit.c              Sphere    src/Sphere.hs:24:9-60              4.2    5.0   2187 1871910656
hit.a              Sphere    src/Sphere.hs:22:9-43              2.8    5.0   1454 1871910656
hit                Sphere    src/Sphere.hs:(51,3)-(64,54)       2.3    3.2   1215 1189647816
hit.halfB          Sphere    src/Sphere.hs:23:9-36              2.0    5.0   1059 1871910656
randomInUnitSphere RtWeekend src/RtWeekend.hs:(46,1)-(50,15)    1.6    4.5    867 1694182016
lambertian         Material  src/Material.hs:(12,1)-(22,19)     1.2    3.1    621 1151823304
randomDoubleR      RtWeekend src/RtWeekend.hs:(34,1)-(38,23)    1.2    2.6    615 956636776
mkWorld            MyLib     src/MyLib.hs:118:1-34              0.9    2.7    476 1022208000
uniformRM          Vec3      src/Vec3.hs:(39,3)-(43,21)         0.8    1.4    412 533407816
metal              Material  src/Material.hs:(25,1)-(34,19)     0.5    1.5    272 571762984
randomScene        MyLib     src/MyLib.hs:(121,1)-(158,65)      0.2    3.6     97 1351965112
```
