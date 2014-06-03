import System.IO
import Control.Concurrent
import qualified Data.ByteString.Char8 as DBC
import Control.Monad
import Web.Twitter.Conduit
import Web.Authenticate.OAuth
import Control.Monad.Logger
import qualified Web.Twitter.Types as TT
import qualified Data.Text as T
import Control.Monad.Error.Class

tokens :: String -> String -> OAuth -- TwitterはアクセスするのにOAuthという仕組みを使っているのですが、その時に必要なConsumerKey/ConsumerSecretを指定します
tokens ck cs = twitterOAuth
    { oauthConsumerKey = DBC.pack ck
    , oauthConsumerSecret = DBC.pack cs
    }
credential :: String -> String -> Credential -- TwitterはアクセスするのにOAuthという仕組みを使っているのですが、その時に必要なAccessToken/AccessTokenSecretを指定します
credential ot ots = Credential
    [ (DBC.pack "oauth_token", DBC.pack ot)
    , (DBC.pack "oauth_token_secret", DBC.pack ots)
    ]
twInfo :: String -> String -> String -> String -> TWInfo -- Twitterにアクセスするのに必要な情報を取得します
twInfo ck cs ot ots = setCredential ( tokens ck cs ) ( credential ot ots ) def

main :: IO ( )
main = do
    putStr "ScreenName(ex. minamiyama1994)を入力 > " >> hFlush stdout
    sn <- getLine
    putStr "ConsumerKeyを入力 > " >> hFlush stdout
    ck <- getLine
    putStr "ConsumerSecretを入力 > " >> hFlush stdout
    cs <- getLine
    putStr "AccessTokenを入力 > " >> hFlush stdout
    ot <- getLine
    putStr "AccessTokenSecretを入力 > " >> hFlush stdout
    ots <- getLine
    putStr "何ふぁぼ以上のツイートを削除せずに残しますか？　nを入力すると、n以上のふぁぼられツイートは削除されません > " >> hFlush stdout
    cnt <- getLine >>= return . read
    ids <- getContents >>= return . map ( read . read ) . words
    forM_ ids $ \i -> flip catchError ( \ _ -> putStrLn $ "ツイート" ++ show i ++ "の削除に失敗しました、処理を続行します" ) $ do
        threadDelay $ 5 * 1000 * 1000
        runNoLoggingT . runTW ( twInfo ck cs ot ots ) $ do
            status <- call $ showId i
            when ( ( TT.statusFavoriteCount status < cnt ) || ( ( TT.userScreenName $ TT.statusUser status ) /= T.pack sn ) ) $ do
                call $ destroyId $ i
                return ( )
        putStrLn $ "ツイート" ++ show i ++ "を削除しました"
    putStrLn $ "処理が終了しました"
