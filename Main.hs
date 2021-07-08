import Morph.Migrator
import Morph.Options

main :: IO ()
main = do
  opts  <- getOptions
  withConnection opts $ \conn -> do
    migrate (optsTransaction opts) conn (optsMigrationsDirectory opts)
