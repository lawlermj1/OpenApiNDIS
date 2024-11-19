
{-# LANGUAGE DataKinds , OverloadedStrings #-} 

import Data.OpenApi
import Data.Either
import Data.Aeson

import qualified Data.ByteString.Lazy as B
import qualified Data.Csv as C 

import qualified Data.HashMap.Strict.InsOrd as IOHM 


-- custom module 
import OpenApiNDIS 

-- | Location of the local copy of the JSON file 
jsonFileFrom :: FilePath
jsonFileFrom = "ndis.swagger.json"

-- Read the local copy of the JSON file 
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

----
main :: IO ()
main = do

    jSONBS <- getJSONFrom

    let oAPI = fromRight mempty (eitherDecode' jSONBS :: Either String OpenApi )

    let schemas = schemasAll oAPI 
    let schemaProps = schemaPropsAll oAPI
    let schemas2 = schemas2All oAPI

    let securitySchemes = securitySchemeAll oAPI 
    let examples = examplesAll oAPI
    let headers = headersAll oAPI 
    let links = linksAll oAPI 
    let responses = responsesAll oAPI 
    let params = paramsAll oAPI 
    let requestbodies = requestBodyAll oAPI 
    let callbacks = callbacksAll oAPI 
    let secReqt = securityRequirementAll oAPI  
    let oas = serversAll oAPI 
    let oaatgs = tagsAll oAPI  
    let pathItems = pathItemsAll oAPI 
    let pathItemSchema = pathItemSchemaAll oAPI 
    let operations = operationsAll oAPI

-- write csv files  
    putStrLn "OpenAPI print"

    B.writeFile "Schemas.csv"  ( C.encodeDefaultOrderedByName  schemas )  
    B.writeFile "SchemaProps.csv"  ( C.encodeDefaultOrderedByName  schemaProps )   

    B.writeFile "Schemas2.csv"  ( C.encodeDefaultOrderedByName  schemas2 )      

    B.writeFile "SecuritySchemes.csv"  ( C.encodeDefaultOrderedByName  securitySchemes)  
    B.writeFile "Examples.csv"  ( C.encodeDefaultOrderedByName  examples)      
    B.writeFile "Headers.csv"  ( C.encodeDefaultOrderedByName  headers)  
    B.writeFile "Links.csv"  ( C.encodeDefaultOrderedByName  links)  
    B.writeFile "Responses.csv"  ( C.encodeDefaultOrderedByName  responses)  
    B.writeFile "Params.csv"  ( C.encodeDefaultOrderedByName  params)  
    B.writeFile "Requestbodies.csv"  ( C.encodeDefaultOrderedByName  requestbodies)  
    B.writeFile "Callbacks.csv"  ( C.encodeDefaultOrderedByName  callbacks)  

    B.writeFile "SecReqt.csv"  ( C.encodeDefaultOrderedByName  secReqt)  
    B.writeFile "Servers.csv"  ( C.encodeDefaultOrderedByName oas )
    B.writeFile "Tags.csv"  ( C.encodeDefaultOrderedByName oaatgs ) 

    B.writeFile "Info.csv"  ( C.encodeDefaultOrderedByName  [oaInfo oAPI])  
    B.writeFile "Version.csv"  ( C.encodeDefaultOrderedByName [oaVer oAPI] )
    B.writeFile "ExternalDocs.csv"  ( C.encodeDefaultOrderedByName [oaExtDocs oAPI] )

    B.writeFile "PathItems.csv"  ( C.encodeDefaultOrderedByName  pathItems )   
    B.writeFile "PathItemSchema.csv"  ( C.encodeDefaultOrderedByName  pathItemSchema )   
    B.writeFile "Operations.csv"  ( C.encodeDefaultOrderedByName  operations )   

