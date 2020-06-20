{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath (takeFileName, replaceDirectory, (</>), (<.>))
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import Data.List.Split (splitOn)
import Data.List.Extra (breakOn)
import Data.Foldable (for_)
import Data.Bifunctor (bimap)
import Control.Monad (when)

snippetTempDir :: FilePath
snippetTempDir = "snippets-temp"

main :: IO ()
main = do
    renderSnippets
    hakyll rules

renderSnippets :: IO ()
renderSnippets = do
    snipRawStr <- readFile "snippets.md"
    let snipStrs = tail $ trim <$> splitOn "===" snipRawStr
    createDirectoryIfMissing False snippetTempDir
    withCurrentDirectory snippetTempDir $ for_ snipStrs \snipStr -> do
        when (null snipStr) $ error "Got empty snippet string"
        let (name, body) = bimap trim trim $ breakOn "---" snipStr
        when (null name) $ error "Got empty snippet name"
        when (null body) $ error "Got empty snippet body"
        writeFile (name <.> "md") body

rules :: Rules ()
rules = do
    match "images/*" do
        route   idRoute
        compile copyFileCompiler

    match "css/*" do
        route   idRoute
        compile compressCssCompiler

    match "pages/*" do
        route $ dropPrefix `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match (fromRegex (snippetTempDir </> "*")) do
        route $ replaceDir "posts" `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["all-posts.html"] do
        route idRoute
        compile do
            postsField <- loadAllPostsField
            let allPostsCtx = mconcat
                    [ postsField
                    , constField "title" "All posts"
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/all-posts.html" allPostsCtx
                >>= loadAndApplyTemplate "templates/default.html" allPostsCtx
                >>= relativizeUrls

    match "index.html" do
        route idRoute
        compile do
            postsField <- loadAllPostsField
            let indexCtx = mconcat
                    [ postsField
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

loadAllPostsField :: Compiler (Context b)
loadAllPostsField = do
    posts <- loadAllPostsAndSnippets >>= recentFirst
    pure $ listField "posts" postCtx (pure posts)

loadAllPostsAndSnippets :: Compiler [Item String]
loadAllPostsAndSnippets = do
    a <- loadAll "posts/*"
    b <- loadAll "snippets-temp/*"
    pure (a <> b)

postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , defaultContext
    ]

dropPrefix :: Routes
dropPrefix = customRoute $
    takeFileName . toFilePath

replaceDir :: String -> Routes
replaceDir d = customRoute $
    flip replaceDirectory d . toFilePath
