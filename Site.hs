{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.List.Extra (breakOn)
import Data.List.Split (splitOn)
import Hakyll
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath (takeFileName, replaceDirectory, (</>), (<.>))

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
    -- Images.
    match "images/*" do
        route   idRoute
        compile copyFileCompiler

    -- CSS.
    match "css/*" do
        route   idRoute
        compile compressCssCompiler

    -- Static pages like 'about' and 'contact'.
    match "pages/*" do
        route $ dropPrefix `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= renderDefault defaultContext

    -- Independent posts.
    match "posts/*" do
        route $ setExtension "html"
        compile postCompiler

    -- Snippet posts.
    match (fromRegex (snippetTempDir </> "*")) do
        route $ replaceDir "posts" `composeRoutes` setExtension "html"
        compile postCompiler

    -- Post list (proper and snippets).
    create ["all-posts.html"] do
        route idRoute
        compile do
            allPostsCtx <- loadAllPostsCtx
            let ctx = mconcat
                    [ allPostsCtx
                    , constField "title" "All posts"
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/all-posts.html" ctx
                >>= renderDefault ctx

    -- Home page.
    match "index.html" do
        route idRoute
        compile do
            allPostsCtx <- loadAllPostsCtx
            let ctx = allPostsCtx <> defaultContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= renderDefault ctx

    -- Templates.
    match "templates/*" $
        compile templateBodyCompiler

postCompiler :: Compiler (Item String)
postCompiler =
    pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= renderDefault postCtx

snippetTempDir :: FilePath
snippetTempDir = "snippets-temp"

renderDefault :: Context String -> Item String -> Compiler (Item String)
renderDefault ctx item =
    loadAndApplyTemplate "templates/default.html" ctx item
        >>= relativizeUrls

loadAllPostsCtx :: Compiler (Context String)
loadAllPostsCtx = do
    posts <- loadAllPostsAndSnippets >>= recentFirst
    pure $ listField "posts" postCtx (pure posts)

loadAllPostsAndSnippets :: Compiler [Item String]
loadAllPostsAndSnippets =
    (<>) <$> loadAll "posts/*" <*> loadAll "snippets-temp/*"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

dropPrefix :: Routes
dropPrefix = customRoute $
    takeFileName . toFilePath

replaceDir :: String -> Routes
replaceDir d = customRoute $
    flip replaceDirectory d . toFilePath
