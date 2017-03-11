module Parser.Mod where

import           Options.Applicative   (Parser (..), auto, execParser, flag, str,
                                        flag', fullDesc, header, help, helper,
                                        info, long, metavar, option, progDesc,
                                        short, showDefault, strArgument, value,
                                        (<>), (<|>), OptionFields(..))
import           Options.Applicative.Builder (subparser, strOption, command, internal, ParseError(..), ArgumentFields(..))
import           Options.Applicative.Builder.Internal ( Mod(..), OptionFields(..), optNames, mkParser)

import           Options.Applicative.Types ( OptReader(..) )


optFlag' :: a -> Mod OptionFields a -> Parser a
optFlag' def m = mkParser d g fdr
    where
        Mod f d g = m
        fields = f (OptionFields [] mempty (ErrorMsg ""))
        fdr = FlagReader (optNames fields) def

optFlag :: a -> a -> Mod OptionFields a -> Parser a
optFlag defv actv m = optFlag' actv m <|> pure defv

{-}
tmp' :: ReadM a -> (a -> b) -> Mod OptionFields a -> Parser b
tmp' act r par fields = optFlag' act <*> argument r fields

tmp :: ReadM a -> (a -> b) -> (a -> b) -> Mod OptionFields a -> Parser b
tmp def act r par fields = optFlag def act <*> argument r fields-}
