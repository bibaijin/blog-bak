--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid               (mappend)
import           Hakyll
import qualified Hakyll.Core.Configuration
import           Hakyll.Web.Pandoc
import           Text.Pandoc.Options
import qualified Data.Set as S


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            firstPost <- return $ take 1 posts
            remainingPosts <- return $ drop 1 posts
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    field "postsCount" (\_ -> return $ show $ length posts) `mappend`
                    listField "firstPost" postCtx (return firstPost) `mappend`
                    listField "remainingPosts" postCtx (return remainingPosts) `mappend`
                    constField "title" "主页" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration { previewHost = "0.0.0.0" }

postCtx :: Context String
postCtx =
  dateField "date" "%Y 年 %m 月 %d 日" `mappend` teaserField "teaser" "content" `mappend`
  defaultContext

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWith readerOptions defaultHakyllWriterOptions
  where customExtensions = [Ext_east_asian_line_breaks]
        defaultExtensions = readerExtensions defaultHakyllReaderOptions
        newExtensions = foldr S.insert defaultExtensions customExtensions
        readerOptions =
          defaultHakyllReaderOptions {readerExtensions = newExtensions}
