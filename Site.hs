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
            >>= renderDefault mempty

    -- Independent posts.
    match "posts/*" do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    dateCtx
            >>= renderDefault dateCtx

    -- Snippet posts.
    match (fromRegex (snippetTempDir </> "*")) do
        route $ replaceDir "posts" `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    dateCtx
            >>= renderDefault dateCtx

    -- Post list (proper and snippets).
    create ["all-posts.html"] do
        route idRoute
        compile do
            postsField <- loadAllPostsField
            let ctx = mconcat
                    [ postsField
                    , constField "title" "All posts"
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/all-posts.html" ctx
                >>= renderDefault ctx

    -- Home page.
    match "index.html" do
        route idRoute
        compile do
            ctx <- loadAllPostsField
            getResourceBody
                >>= applyAsTemplate ctx
                >>= renderDefault ctx

    -- Templates.
    match "templates/*" $
        compile templateBodyCompiler

snippetTempDir :: FilePath
snippetTempDir = "snippets-temp"

renderDefault :: _ -> Compiler _
renderDefault ctx =
    loadAndApplyTemplate "templates/default.html" (ctx <> defaultContext)
        >>= relativizeUrls

loadAllPostsField :: Compiler (Context b)
loadAllPostsField = do
    posts <- loadAllPostsAndSnippets >>= recentFirst
    pure $ listField "posts" dateCtx (pure posts)

loadAllPostsAndSnippets :: Compiler [Item String]
loadAllPostsAndSnippets =
    (<>) <$> loadAll "posts/*" <*> loadAll "snippets-temp/*"

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y"

dropPrefix :: Routes
dropPrefix = customRoute $
    takeFileName . toFilePath

replaceDir :: String -> Routes
replaceDir d = customRoute $
    flip replaceDirectory d . toFilePath
