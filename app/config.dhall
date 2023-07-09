let Config =
      { Type =
          { imageWidth : Optional Natural, samplesPerPixel : Optional Natural }
      , default = { imageWidth = Some 400, samplesPerPixel = Some 100 }
      }

in  Config.default
