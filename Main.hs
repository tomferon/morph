import Morph.Config
import Morph.Migrator
import Morph.Options

main :: IO ()
main = do
  opts   <- getOptions
  config <- readConfigOrDie opts
  withConnection config $ \conn -> do
    migrate (optsTransaction opts) conn (optsMigrationsDirectory opts)
