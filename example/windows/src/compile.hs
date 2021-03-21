import Control.Monad
import Development.Shake
import Development.Shake.FilePath

main = shake shakeOptions $ do
    want ["bin/lib/lib.o", "bin/lib/lib-copy.o"]

    "bin//*.o" %> \obj -> do
        let src = "src/lib" </> takeFileName obj -<.> "c"
        need [src]
        cmd "gcc" ["-c", src, "-Isrc/lib", "-o", obj]
