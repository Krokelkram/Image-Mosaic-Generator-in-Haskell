module Menu where
import System.Console.CmdArgs
import Types

-- This is the definition for the analyser mode
-- it specifies all the default values and help texts that are used by CmdArgs to show the command line options
analyser :: Mode Mosa
analyser = mode $ Analyse
    {src = "." &= text "Path to your image folder" & typDir,
           dbPath = "./mosaicTiles" &= text "MosaicMaker will generate DB.txt into this folder \n\t\t\t(DEFAULT '.' STRONGLY RECOMMENDED)" & typDir
    } &= prog "MosaicMaker" & text "Create database"

--srcFlags :: Attrib
--srcFlags = text "Source folder" & typFile

-- This is the definition for the generator mode
-- it specifies all the default values and help texts that are used by CmdArgs to show the command line options
-- format is "flagname = 'defaultvalue' &= text 'helptext' & 'type of that flag'
generator :: Mode Mosa
generator = mode $ Generate
            {from = "." &= text "Image that will be made into a mosaic" & typFile, 
                    out = "mosaic.jpg" &= text "Output file" & typFile,
                    dbPath_ = "./mosaicTiles" &= text "Path to DB.txt \n\t\t\t(DEFAULT '.' STRONGLY RECOMMENDED)" & typDir,
                    columns = 64 &= text "Number of horizontal tiles" & typ "DECIMAL",
                    rows = 64 &= text "Number of vertical tiles" & typ "DECIMAL"
            }  &= prog "MosaicMaker" & text "Generate a mosaic from files in database"


-- cmdArgs needs a list of all modes available
-- here: analyser and generator mode
modes :: [Mode Mosa]
modes = [analyser, generator]


