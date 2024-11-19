
{-# LANGUAGE DataKinds , OverloadedStrings #-} 

{- |
   Module      : NDIS OpenAPI parsing 
   Description : Parsing OpenAPI JSON file defined by NDIS 
   Copyright   : ( c ) Matthew Lawler 2024 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module parses the NDIS OpenAPI specification file. 

   Ref: https://www.ndis.gov.au/providers/working-provider/connecting-ndia-systems 

  Design Patterns used. 
  ----
  There are 2 tricky ones, Inspecting them requires some deep type dives. 

  1. Definitions 

  type Definitions = InsOrdHashMap Text
  _componentsSchemas :: Definitions Schema 

  This is a partial application of InsOrdHashMap. 
  The Text represents the key value for whatever the second type is. 
  So _componentsSchemas can be truned into a (k,v) = (Text,Schema). 

  2. Referenced 
   data Referenced a = Ref Reference | Inline a 
   _pathItemParameters :: [Referenced Param]

  This is really a kind of Either. 
  It allows the specification of a reference or an instance of a type. 
  In schemas, it can be thought of as a reference to another table, or a field definition. 
  In the spec, these are called $ref 

  3. Aeson 
encode and decode are Aeson commands
needs aeson and swagger 
encode (mempty :: Swagger)
"{\"swagger\":\"2.0\",\"info\":{\"title\":\"\",\"version\":\"\"}}"
decode $ encode (mempty :: Swagger)
Just ()
encode :: ToJSON a => a -> ByteString 
encodeFile :: ToJSON a => FilePath -> a -> IO ()
decodeStrict :: FromJSON a => ByteString -> Maybe a 
decodeFileStrict :: FromJSON a => FilePath -> IO (Maybe a)

  4. Schemas and Objects 
  Treat Object is an entity type. It can also be an OO object with a method! E.g. AddGroup. 
  Schema properties are Entity attributes. 

Background 
----
OpenAPI was originally called swagger. 
https://swagger.io/specification/

Nickolay Kudasov has developed the Swagger library, whihc is now replaced by the OpenAPi libray. 

Other swagger and api links: 

https://editor.swagger.io/

https://apis.guru/

Done
---- 
0.	Text 
Data.HashMap.Strict.InsOrd as IOHM  insert-ordered-containers
Data.Text text

1.	Use swagger to discover hs types 
1a.	swagger -> paramschema cvs
1b. swagger -> schema csv (per path)
2.	swagger -> csv 

to do 
----
Parse out key words from the name e.g. AddGroup => Add Group 

3.	swagger to DDL 
4.	encode swagger -> Hs Type 
5.	diff swagger with original until all parsed use compaREST 
https://github.com/typeable/compaREST 
6.  API code genration 

Code generation
----
Parser here
https://github.com/owainlewis/openapi/tree/master

Code generator here 
https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator

Next level codegen using servant 
https://github.com/swagger-api/swagger-codegen

https://github.com/OpenAPITools/openapi-generator/blob/master/docs/generators/haskell.md

https://github.com/OpenAPITools/openapi-generator/blob/master/docs/generators/haskell.md

 -- Issues - closed 
-- 0. JSON requires serious, byzantine type foo 
-- 1. Couldn't match type ‘http-media-0.8.1.1:Network.HTTP.Media.MediaType.Internal.MediaType’ with ‘MediaType’
--rb2Medias :: Referenced RequestBody -> [(MediaType, MediaTypeObject)] 
--rb2Medias rrb = map (\(k,v) -> (k, mto2ref v)) (IOHM.toList (_requestBodyContent rrb)) 
-- Solved by not defining the type - relaxing the function definition 

-- 2. Number parsing for Enum values 
-- Resolved through loss typing, and string parsing 

-- 3. Scientific module mismatch - coerce does not work 
-- package match error error 
-- This seems to be caused by using ! for strictness on the Value type. 
-- Couldn't match expected type `Scientific'  with actual type `scientific-0.3.7.0:Data.Scientific.Scientific' 
-- ignored as there is a show instance anyway 

-- Issues - open 
-- 10. Incomplete parsing due to unspecified types by ndis at the top level. 
--   e.g. Callbacks, Examples, ExternalDocs, Headers, Links, Responses, Servers, Tags 
-- some of these occur under PathItems. 
-- all Responses are always 200 - not other http codes defined. 
-- a smale number of paths have multiple operations. 
-- Most of the are just one, such as get, put, etc. 

-- 11. Link - Not in scope: type constructor or class ‘ExpressionOrValue’  
-- Not in scope: type constructor or class ‘ExpressionOrValue’ 
--defaultExpressionOrValue :: ExpressionOrValue 
--defaultExpressionOrValue = defaultValue 

 -}

module OpenApiNDIS    
    (  
      
      schemasAll, 
      schemaPropsAll, 
      schemas2All, 

      securitySchemeAll, 
      examplesAll, 
      headersAll, 
      linksAll, 
      responsesAll, 
      paramsAll, 
      requestBodyAll,
      callbacksAll, 
      securityRequirementAll, 
      serversAll, 
      tagsAll, 
      pathItemsAll, 
      pathItemSchemaAll, 
      operationsAll, 

      oaInfo, 
      oaVer, 
      oaExtDocs, 

     ) where

import Data.Either
import Data.Aeson
import Data.Coerce
import Data.Maybe
import Data.List
import GHC.Generics
import Data.OpenApi
import Data.Scientific
import Data.Version
import Network.HTTP.Media.MediaType 
import Data.List.Split 
import Data.Bifunctor

import qualified Data.Text as T 
import qualified Data.Csv as C 
import qualified Data.Csv.Builder as CB 
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.HashSet.InsOrd as IOHS 
import Data.Text (Text)

----
-- Utility
commaText :: [T.Text] -> T.Text 
commaText ts  
    | lt == 0 = "" 
    | lt == 1 = head ts 
    | otherwise = T.intercalate "," ts      
    where 
        lt = length ts 

showBlank :: (Show a) => Maybe a -> String 
showBlank b
    | isJust b = show b 
    | otherwise = "" 

b2i :: Bool -> Integer
b2i b = if b then 1 else 0 

show01 :: Bool -> String 
show01 b = if b then "1" else "0" 

getRefType :: Referenced a -> T.Text 
getRefType (Inline a) = "Inline"  
getRefType (Ref x) = "Ref" 

isRef01 :: Referenced a -> Int 
isRef01 (Inline a) = 0 
isRef01 (Ref x) = 1 

isRef :: Referenced a -> Bool   
isRef (Inline a) = False 
isRef (Ref x) = True 

unwrapRef :: Referenced a -> T.Text 
unwrapRef (Ref x) = getReference x  
unwrapRef  a = ""

defaultRefAny :: Referenced a
defaultRefAny = Ref (Reference "") 

----
-- defaults for OpenAPI Types 
defaultContact :: Contact
defaultContact = Contact Nothing Nothing Nothing

defaultExample :: Example 
defaultExample = Example Nothing Nothing Nothing Nothing 

defaultExternalDocs :: ExternalDocs
defaultExternalDocs = ExternalDocs Nothing defaultURL 

defaultHeader :: Header
defaultHeader = Header Nothing Nothing Nothing Nothing Nothing Nothing mempty Nothing 

defaultLicense :: License
defaultLicense = License "" Nothing 

defaultLink :: Link
defaultLink = Link Nothing Nothing mempty Nothing Nothing Nothing 

defaultOperation :: Operation
defaultOperation = Operation mempty Nothing Nothing Nothing Nothing [] Nothing mempty mempty Nothing [] [] 

defaultParam :: Param  
defaultParam = Param "" Nothing Nothing Nothing ParamCookie Nothing Nothing Nothing Nothing Nothing Nothing mempty 

defaultRequestBody :: RequestBody 
defaultRequestBody = RequestBody Nothing mempty Nothing 

defaultServer :: Server 
defaultServer = Server "" Nothing mempty 

defaultSchema :: Schema
defaultSchema = 
    Schema Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing mempty Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

defaultURL :: URL 
defaultURL = URL "" 

defaultValue :: Value 
defaultValue = Null 
---- 
-- Components Schema 
data SchemaObjectData  = 
  SchemaObjectData { 
            schemaObjectKey :: String 
          , schemaObjectType :: String
          , schemaObjectFormat :: String
          , schemaObjectNullable :: String       
          , schemaObjectPropertyNames :: String    
          , schemaObjectPropertyCount :: String            
          , schemaObjectPropertyRefCount :: String    
          , schemaObjectPropertyInlineCount :: String

          , compschemaRequired :: String
          , compschemaEnum :: String 
          , compsMaybeBool :: String           
          , compschemaAllOf :: String
          , compschemaAnyOf :: String
          , compschemaOneOf :: String
          , compschemaDeprecated :: String
          , compschemaExclusiveMaximum :: String
          , compschemaExclusiveMinimum :: String
          , compschemaReadOnly :: String
          , compschemaUniqueItems :: String
          , compschemaWriteOnly :: String
          , compschemaMaxItems :: String
          , compschemaMaxLength :: String
          , compschemaMaxProperties :: String
          , compschemaMinItems :: String
          , compschemaMinLength :: String
          , compschemaMinProperties :: String
          , compschemaPattern :: String
          , compschemaDescription :: String
          , compschemaTitle :: String
          , compschemaDefault :: String
          , compschemaExample :: String
          , compschemaNot :: String
          , compschemaAdditionalProperties :: String
          , compschemaDiscriminator  :: String
          , compschemaExternalDocs :: String
          , compschemaItems :: String
          , compschemaXml :: String
          , compschemaMaximum :: String
          , compschemaMinimum :: String 
          , compschemaMultipleOf :: String 

          } deriving (Show, Generic) 
defaultSchemaObjectData 
    = SchemaObjectData "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" 
    "" "" "" "" "" "" "" "" "" "" "" "" "" "" 

instance C.ToRecord SchemaObjectData
instance C.FromRecord SchemaObjectData

instance C.FromNamedRecord SchemaObjectData
instance C.ToNamedRecord SchemaObjectData
instance C.DefaultOrdered SchemaObjectData 

data OASchema2  = 
  OASchema2 { 
            s_Object :: String 
          , s_Object_Type :: String
          , s_Object_Format :: String
          , s_Object_Prop_Count :: String            
          , s_Object_Prop_Ref_Count :: String    
          , s_Enum_List :: String 
          , s_Enum_Bool :: String    
          , s_Object_Prop_Names :: String   
 
          } deriving (Show, Generic) 
defaultOASchema2 
    = OASchema2 "" "" "" "" "" "" "" "" 

instance C.ToRecord OASchema2
instance C.FromRecord OASchema2

instance C.FromNamedRecord OASchema2
instance C.ToNamedRecord OASchema2
instance C.DefaultOrdered OASchema2 

unwrapSchema :: Referenced Schema -> Schema 
unwrapSchema (Inline a) = a  
unwrapSchema (Ref x) = defaultSchema 

defaultReferencedSchema :: Referenced Schema
defaultReferencedSchema = Inline defaultSchema 
----

oa2sc :: OpenApi -> [(T.Text,Schema)] 
oa2sc o = IOHM.toList (coerce (_componentsSchemas (_openApiComponents o))) 

isOpenApiObject :: Schema -> Bool
isOpenApiObject s = OpenApiObject == fromMaybe OpenApiNull (_schemaType s)

filterObj :: [(T.Text,Schema)] -> [(T.Text,Schema)] 
filterObj = filter (\(k,v) -> isOpenApiObject v) 

schemaPropertyNames :: Schema ->  [T.Text] 
schemaPropertyNames = IOHM.keys . _schemaProperties 

schemaPropertyRefs :: Schema ->  Int
schemaPropertyRefs v = sum (map isRef01 (IOHM.elems (_schemaProperties v)))   

schemaType :: Schema -> OpenApiType
schemaType v = fromMaybe OpenApiNull (_schemaType v) 

schemaFormat :: Schema -> Format  
schemaFormat v = fromMaybe "" (_schemaFormat v)  

schemaAllOf :: Schema -> [Referenced Schema]
schemaAllOf v = fromMaybe [] ( _schemaAllOf v)
schemaOneOf :: Schema -> [Referenced Schema]
schemaOneOf v = fromMaybe [] ( _schemaOneOf v)
schemaAnyOf :: Schema -> [Referenced Schema]
schemaAnyOf v = fromMaybe [] ( _schemaAnyOf v) 

defaultAdditionalProperties :: Maybe AdditionalProperties
defaultAdditionalProperties = Just (AdditionalPropertiesAllowed False)  

showAP :: Schema -> String  
showAP s
    | isNothing ap = "" 
    | ap == defaultAdditionalProperties = "" 
    | otherwise = show ap 
    where ap = _schemaAdditionalProperties s 

schemaEnum :: Schema -> [Value] 
schemaEnum v = fromMaybe [] (_schemaEnum v) 

showNumber :: Value -> String 
showNumber (Number n) = show n 

schemaEnums :: Schema -> [String]
--schemaEnums v = map (fst.break (== '.').showNumber) (schemaEnum v) 
schemaEnums v = map (takeWhile (/= '.').showNumber) (schemaEnum v) 

maybeBool :: [String] -> Bool
maybeBool =  (["0","1"] ==) 

schema2DataObject :: (T.Text, Schema) -> SchemaObjectData 
schema2DataObject (k,v) = 
       SchemaObjectData 
            (show k)
            (show (schemaType v)) 
            (show (schemaFormat v))  
            (show01 (fromMaybe False (_schemaNullable v))) 
            (show (schemaPropertyNames v)) 
            (show ls) 
            (show rs) 
            (show (subtract rs ls)) 

            (show (_schemaRequired v)) 
            (show (schemaEnums v))    
            (show01 (maybeBool (schemaEnums v)))                      
            (show (schemaAllOf v)) 
            (show (schemaAnyOf v)) 
            (show (schemaOneOf v)) 
            (show01 (fromMaybe False ( _schemaDeprecated v))) 
            (show01 (fromMaybe False ( _schemaExclusiveMaximum v))) 
            (show01 (fromMaybe False ( _schemaExclusiveMinimum v))) 
            (show01 (fromMaybe False (_schemaReadOnly v))) 
            (show01 (fromMaybe False ( _schemaUniqueItems v))) 
            (show01 (fromMaybe False ( _schemaWriteOnly v))) 
            (show (fromMaybe 0 ( _schemaMaxItems v))) 
            (show (fromMaybe 0 ( _schemaMaxLength v))) 
            (show (fromMaybe 0 ( _schemaMaxProperties v))) 
            (show (fromMaybe 0 ( _schemaMinItems v))) 
            (show (fromMaybe 0 ( _schemaMinLength v))) 
            (show (fromMaybe 0 ( _schemaMinProperties v))) 
            (show (fromMaybe "" ( _schemaPattern v))) 
            (show (fromMaybe "" ( _schemaDescription v))) 
            (show (fromMaybe "" ( _schemaTitle v))) 
            (show (fromMaybe Null ( _schemaDefault v))) 
            (show (fromMaybe Null ( _schemaExample v))) 
            (showBlank (_schemaNot v)) 
            (showAP v)   
            (showBlank (_schemaDiscriminator  v)) 
            (showBlank (_schemaExternalDocs v)) 
            (showBlank (_schemaItems v)) 
            (showBlank (_schemaXml v)) 
            (showBlank ( _schemaMaximum v))  
            (showBlank ( _schemaMinimum v))  
            (showBlank ( _schemaMultipleOf v))  

            where 
                ls = length (schemaPropertyNames v) 
                rs = schemaPropertyRefs v 

schemasAll :: OpenApi -> [SchemaObjectData] 
schemasAll oAPI = map schema2DataObject (oa2sc oAPI)  

schema2Schema2 :: (T.Text, Schema) -> OASchema2 
schema2Schema2 (k,v) = 
       OASchema2 
            (show k)
            (drop 7 (show (schemaType v)))  
            (show (schemaFormat v))  
            (show ls) 
            (show rs) 
            (show (schemaEnums v))    
            (show01 (maybeBool (schemaEnums v)))      
            (show (schemaPropertyNames v)) 
            where 
                ls = length (schemaPropertyNames v) 
                rs = schemaPropertyRefs v 

schemas2All :: OpenApi -> [OASchema2] 
schemas2All oAPI = map schema2Schema2 (oa2sc oAPI)  

---- 
-- Components Schema Properties 
data SchemaObjectPropData  = 
  SchemaObjectPropData { 
            sp_Object :: String 
          , sp_Property :: String
          , sp_Reference :: String  
          , sp_Type :: String
          , sp_Format :: String
          , sp_Nullable :: String     
          , sp_Reqd :: String
          , sp_Enum :: String
          , sp_Props :: String          

          } deriving (Show, Generic) 

defaultSchemaObjectPropData = SchemaObjectPropData "" "" "" "" "" ""  "" "" "" 

instance C.ToRecord SchemaObjectPropData
instance C.FromRecord SchemaObjectPropData

instance C.FromNamedRecord SchemaObjectPropData
instance C.ToNamedRecord SchemaObjectPropData
instance C.DefaultOrdered SchemaObjectPropData 

object2Prop :: (T.Text, Schema) -> [(T.Text, T.Text, Referenced Schema)] 
object2Prop (k,v) = 
    map (\(l,w) -> (k,l,w)) (IOHM.toList (_schemaProperties v)) 

schema2DataPropObject :: (T.Text, T.Text, Referenced Schema) -> SchemaObjectPropData 
schema2DataPropObject (k,l,v) = 
       SchemaObjectPropData 
            (show k) 
            (show l) 
            (show (unwrapRef v))  
            (drop 7 (show (schemaType s)))  
            (show (schemaFormat s))  
            (show01 (fromMaybe False (_schemaNullable s)))    
            (show (_schemaRequired s)) 
            (show (schemaEnum s)) 
            (show (_schemaProperties s)) 
            where         
                s = unwrapSchema v 

schemaPropsAll :: OpenApi -> [SchemaObjectPropData] 
schemaPropsAll oAPI = map schema2DataPropObject (concatMap object2Prop (filterObj (oa2sc oAPI))) 

---- 
-- Components SecurityDefinitions SecurityScheme
data OaSecurityScheme  = 
  OaSecurityScheme { 
            oaSecuritySchemeKey :: String     
          , oaSecuritySchemeType :: String 
          , oaSecuritySchemeTypeEncode :: String           
          , oaSecuritySchemeDescription :: String
            } deriving (Show, Generic) 

defaultOaSecurityScheme :: OaSecurityScheme
defaultOaSecurityScheme = OaSecurityScheme "" "" "" "" 

instance C.ToRecord OaSecurityScheme
instance C.FromRecord OaSecurityScheme

instance C.FromNamedRecord OaSecurityScheme
instance C.ToNamedRecord OaSecurityScheme
instance C.DefaultOrdered OaSecurityScheme 

oa2sd :: OpenApi -> SecurityDefinitions
oa2sd = _componentsSecuritySchemes._openApiComponents

sd2dss2 :: SecurityDefinitions -> IOHM.InsOrdHashMap T.Text SecurityScheme  
sd2dss2 (SecurityDefinitions d) = coerce d 

sd2ssr :: (T.Text, SecurityScheme) -> OaSecurityScheme 
sd2ssr (k,v) = 
       OaSecurityScheme 
            (show k)
            (show (_securitySchemeType v))  
            (show (encode (_securitySchemeType v))) 
            (show (fromMaybe "" (_securitySchemeDescription v)))             

securitySchemeAll :: OpenApi -> [OaSecurityScheme]
securitySchemeAll oAPI = map sd2ssr (IOHM.toList ((sd2dss2.oa2sd) oAPI)) 

----
-- Components Response  
data OaResponse = 
  OaResponse { 
        oaResponseKey :: String 
      , oaResponseDescription :: String 
      , oaResponseContent :: String 
      , oaResponseHeaders :: String 
      , oaResponseLinkss :: String       
      } deriving (Show, Generic) 

defaultOaResponse = OaResponse "" "" "" "" "" 

instance C.ToRecord OaResponse
instance C.FromRecord OaResponse

instance C.FromNamedRecord OaResponse
instance C.ToNamedRecord OaResponse
instance C.DefaultOrdered OaResponse 

oa2res :: OpenApi -> [(T.Text,Response)]  
oa2res o = IOHM.toList (coerce (_componentsResponses (_openApiComponents o)))   

rc2list :: IOHM.InsOrdHashMap MediaType MediaTypeObject -> [(MediaType,MediaTypeObject)] 
rc2list = IOHM.toList  

rh2list :: IOHM.InsOrdHashMap HeaderName (Referenced Header) -> [(HeaderName,Referenced Header)] 
rh2list = IOHM.toList  

rl2list :: IOHM.InsOrdHashMap T.Text (Referenced Link) -> [(T.Text,Referenced Link)] 
rl2list = IOHM.toList 

-- isRef :: Referenced a -> Bool   
-- unwrapRef :: Referenced a -> T.Text 
-- defaultRefAny :: Referenced a

res2resr :: (T.Text,Response) -> OaResponse 
res2resr (k,v) = 
  OaResponse 
    (show k)
    (show (_responseDescription v))  
    (show (_responseContent v))  
    (show (rh2list (_responseHeaders v)))   
    (show (rl2list (_responseLinks v)))  

responsesAll :: OpenApi -> [OaResponse] 
responsesAll oAPI = map res2resr (oa2res oAPI) 

----
-- Components Param 
data OaParam = 
  OaParam { 
        oaParamKey :: String     
      , oaParamName :: String 
      , oaParamDescription :: String 
      , oaParamRequired :: String 
      , oaParamDeprecated :: String 
      , oaParamIn :: String 
      , oaParamAllowEmptyValue :: String 
      , oaParamAllowReserved :: String 
      , oaParamSchema :: String 
      , oaParamStyle :: String 
      , oaParamExplode :: String 
      , oaParamExample :: String 
      , oaParamExamples :: String 
      } deriving (Show, Generic) 

defaultOaParam = OaParam "" "" "" "" "" "" "" "" "" "" "" "" 

instance C.ToRecord OaParam
instance C.FromRecord OaParam

instance C.FromNamedRecord OaParam
instance C.ToNamedRecord OaParam
instance C.DefaultOrdered OaParam 

oa2pm :: OpenApi -> [(T.Text,Param)]  
oa2pm o = IOHM.toList (coerce (_componentsParameters (_openApiComponents o)))   

-- populated _paramName _paramIn _paramSchema 
pm2pmr :: (T.Text,Param) -> OaParam 
pm2pmr (k,p)  = 
  OaParam 
    (show k) 
    (show (_paramName p))  
    (show (fromMaybe "" (_paramDescription p)))  
    (show01 (fromMaybe False (_paramRequired p)))      
    (show01 (fromMaybe False (_paramDeprecated p)))  
    (show (_paramIn p))      
    (show01 (fromMaybe False (_paramAllowEmptyValue p)))  
    (show01 (fromMaybe False (_paramAllowReserved p)))  
    (show (fromMaybe defaultReferencedSchema (_paramSchema p)))  
    (show (fromMaybe StyleSimple (_paramStyle p)))      
    (show01 (fromMaybe False (_paramExplode p)))  
    (show (fromMaybe Null (_paramExample p)))   
    (show (_paramExamples p))      

paramsAll :: OpenApi -> [OaParam]
paramsAll oAPI = map pm2pmr (oa2pm oAPI) 

----
-- Components Example 
data OaExample = 
  OaExample { 
        oaExampleKey :: String 
      , oaExampleSummary :: String 
      , oaExampleDescription :: String 
      , oaExampleValue :: String 
      , oaExampleExternalValue :: String 
      } deriving (Show, Generic) 

defaultOaExample = OaExample "" "" "" "" "" 

instance C.ToRecord OaExample
instance C.FromRecord OaExample

instance C.FromNamedRecord OaExample
instance C.ToNamedRecord OaExample
instance C.DefaultOrdered OaExample 

unwrapExample :: Referenced Example -> Example 
unwrapExample (Inline a) = a  
unwrapExample (Ref x) = defaultExample 

defaultReferencedExample :: Referenced Example
defaultReferencedExample = Inline defaultExample 

oa2ex :: OpenApi -> [(T.Text,Example)]  
oa2ex o = IOHM.toList (coerce (_componentsExamples (_openApiComponents o)))   

ex2exr :: (T.Text,Example) -> OaExample 
ex2exr (k,v) = 
  OaExample 
    (show k)
    (show (fromMaybe "" (_exampleSummary v)))  
    (show (fromMaybe "" (_exampleDescription v)))  
    (show (fromMaybe Null (_exampleValue v))) 
    (show (fromMaybe defaultURL (_exampleExternalValue v))) 

examplesAll :: OpenApi -> [OaExample] 
examplesAll oAPI = map ex2exr (oa2ex oAPI)  

----
-- Components RequestBody  
data OaRequestBody = 
  OaRequestBody { 
        oaRequestBodyKey :: String 
      , oaRequestBodyDescription :: String 
      , oaRequestBodyContent :: String 
      , oaRequestBodyRequired :: String 
      } deriving (Show, Generic) 

defaultOaRequestBody = OaRequestBody "" "" "" "" 

instance C.ToRecord OaRequestBody
instance C.FromRecord OaRequestBody

instance C.FromNamedRecord OaRequestBody
instance C.ToNamedRecord OaRequestBody
instance C.DefaultOrdered OaRequestBody 

defaultReferenceRequestBody :: Referenced RequestBody 
defaultReferenceRequestBody = Inline defaultRequestBody 

unwrapRequestBody :: Referenced RequestBody -> RequestBody 
unwrapRequestBody (Inline a) = a  
unwrapRequestBody (Ref x) = defaultRequestBody 

oa2rb :: OpenApi -> [(T.Text,RequestBody)]  
oa2rb o = IOHM.toList (coerce (_componentsRequestBodies (_openApiComponents o)))   

rb2rbr :: (T.Text,RequestBody) -> OaRequestBody 
rb2rbr (k,v) = 
  OaRequestBody 
    (show k)
    (show (fromMaybe "" (_requestBodyDescription v)))  
    (show (_requestBodyContent v))    
    (show01 (fromMaybe False (_requestBodyRequired v))) 

requestBodyAll :: OpenApi -> [OaRequestBody]
requestBodyAll oAPI = map rb2rbr (oa2rb oAPI) 

----
-- Components Header  
data OaHeader = 
  OaHeader { 
        oaHeaderKey :: String 
      , oaHeaderDescription :: String 
      , oaHeaderRequired :: String 
      , oaHeaderDeprecated :: String 
      , oaHeaderAllowEmptyValue :: String       
      , oaHeaderExplode :: String 
      , oaHeaderExample :: String 
      , oaHeaderExamples :: String 
      , oaHeaderSchemaRef :: String
      , oaHeaderSchemaInline :: String      
      } deriving (Show, Generic) 

defaultOaHeader = OaHeader "" "" "" "" "" "" "" "" "" "" 

instance C.ToRecord OaHeader
instance C.FromRecord OaHeader

instance C.FromNamedRecord OaHeader
instance C.ToNamedRecord OaHeader
instance C.DefaultOrdered OaHeader 

unwrapHeader :: Referenced Header -> Header 
unwrapHeader (Inline a) = a  
unwrapHeader (Ref x) = defaultHeader 

defaultReferencedHeader :: Referenced Header
defaultReferencedHeader = Inline defaultHeader 

oa2h :: OpenApi -> [(T.Text,Header)]  
oa2h o = IOHM.toList (coerce (_componentsHeaders (_openApiComponents o)))   

h2hr :: (T.Text,Header) -> OaHeader 
h2hr (k,v) = 
  OaHeader 
    (show k)
    (show (fromMaybe "" (_headerDescription v)))  
    (show01 (fromMaybe False (_headerRequired v)))  
    (show01 (fromMaybe False (_headerDeprecated v))) 
    (show01 (fromMaybe False (_headerAllowEmptyValue v))) 
    (show01 (fromMaybe False (_headerExplode v))) 
    (show (fromMaybe Null (_headerExample v))) 
-- needs more parsing        
    (show (_headerExamples v)) 
    (show (if isRef hsref then unwrapRef hsref else ""))  
    (show (if isRef hsinl then defaultSchema else unwrapSchema hsinl))  
    where 
      hsref = fromMaybe defaultRefAny (_headerSchema v) 
      hsinl = fromMaybe defaultReferencedSchema (_headerSchema v) 

headersAll :: OpenApi -> [OaHeader] 
headersAll oAPI = map h2hr (oa2h oAPI) 

----
-- Components Link 
data OaLink = 
  OaLink { 
        oaLinkKey :: String 
      , oaLinkOperationRef :: String 
      , oaLinkOperationId :: String 
      , oaLinkParameters :: String 
      , oaLinkRequestBody :: String       
      , oaLinkDescription :: String 
      , oaLinkServer :: String    
      } deriving (Show, Generic) 

defaultOaLink = OaLink "" "" "" "" "" "" "" 

instance C.ToRecord OaLink
instance C.FromRecord OaLink

instance C.FromNamedRecord OaLink
instance C.ToNamedRecord OaLink
instance C.DefaultOrdered OaLink 

unwrapLink :: Referenced Link -> Link 
unwrapLink (Inline a) = a  
unwrapLink (Ref x) = defaultLink 

defaultReferencedLink :: Referenced Link
defaultReferencedLink = Inline defaultLink 

oa2l :: OpenApi -> [(T.Text,Link)]  
oa2l o = IOHM.toList (coerce (_componentsLinks (_openApiComponents o)))   

l2lr :: (T.Text,Link) -> OaLink
l2lr (k,v) = 
  OaLink
    (show k)
    (show (fromMaybe "" (_linkOperationRef v)))  
    (show (fromMaybe "" (_linkOperationId v)))  
    (show (_linkParameters v))       
--    (show (fromMaybe Null (_linkRequestBody v)))   
    (show "ExpressionOrValue issue") 
    (show (fromMaybe "" (_linkDescription v))) 
    (show (fromMaybe defaultServer (_linkServer v)))     

linksAll :: OpenApi -> [OaLink]
linksAll oAPI = map l2lr (oa2l oAPI) 

----
-- Components Callback 
data OaCallback = 
  OaCallback { 
        oaCallbackKey :: String 
      , oaCallbackPathItems :: String 
      } deriving (Show, Generic) 

defaultOaCallback = OaCallback "" "" 

instance C.ToRecord OaCallback
instance C.FromRecord OaCallback

instance C.FromNamedRecord OaCallback
instance C.ToNamedRecord OaCallback
instance C.DefaultOrdered OaCallback 

oa2cb :: OpenApi -> [(T.Text,Callback)]  
oa2cb o = IOHM.toList (coerce (_componentsCallbacks (_openApiComponents o)))   

getCB :: Callback -> IOHM.InsOrdHashMap Text PathItem 
getCB (Callback cb) = cb

cb2cbr :: (T.Text,Callback) -> OaCallback
cb2cbr (k,v) = 
  OaCallback
    (show k)
    (show (getCB v))   

callbacksAll :: OpenApi -> [OaCallback]
callbacksAll oAPI = map cb2cbr (oa2cb oAPI) 

----
-- SecurityRequirement 
data OaSecurityReqt  = 
  OaSecurityReqt { 
            oaSecurityReqtKey :: String     
          , oaSecurityReqtValue :: String 
            } deriving (Show, Generic) 

defaultOaSecurityReqt = OaSecurityReqt "" "" 

instance C.ToRecord OaSecurityReqt
instance C.FromRecord OaSecurityReqt

instance C.FromNamedRecord OaSecurityReqt
instance C.ToNamedRecord OaSecurityReqt
instance C.DefaultOrdered OaSecurityReqt 

oa2sr :: OpenApi -> [(T.Text,[T.Text])] 
oa2sr o = concatMap (IOHM.toList.getSecurityRequirement) (_openApiSecurity o) 

sr2srr :: (T.Text, [T.Text]) -> OaSecurityReqt  
sr2srr (k,v) = OaSecurityReqt (show k) (show v)  

securityRequirementAll :: OpenApi -> [OaSecurityReqt]
securityRequirementAll oAPI = map sr2srr (oa2sr oAPI)

----
-- Server 
data OaServer = 
  OaServer { 
        oaServerUrl :: String 
      , oaServerDescription :: String 
      , oaServerVariables :: String 
      } deriving (Show, Generic) 

defaultOaServer = OaServer "" "" "" 

instance C.ToRecord OaServer
instance C.FromRecord OaServer

instance C.FromNamedRecord OaServer
instance C.ToNamedRecord OaServer
instance C.DefaultOrdered OaServer 

oa2sv :: Server -> OaServer 
oa2sv s = 
  OaServer 
    (show (_serverUrl s)) 
    (show (_serverDescription s)) 
    (show (_serverVariables s)) 

serversAll :: OpenApi -> [OaServer]
serversAll oAPI = map oa2sv (_openApiServers oAPI)

----
-- Tag 
data OaTags = 
  OaTags { 
        oaTagName :: String 
      , oaTagDescription :: String 
      , oaTagExternalDocs :: String 
      } deriving (Show, Generic) 

defaultOaTags = OaTags "" "" "" 

instance C.ToRecord OaTags
instance C.FromRecord OaTags

instance C.FromNamedRecord OaTags
instance C.ToNamedRecord OaTags
instance C.DefaultOrdered OaTags 

oa2t :: OpenApi -> [Tag] 
oa2t = IOHS.toList._openApiTags 

t2tr :: Tag -> OaTags 
t2tr t = 
  OaTags 
    (show (_tagName t )) 
    (show (fromMaybe "" (_tagDescription t))) 
    (show (fromMaybe defaultExternalDocs ( _tagExternalDocs t)))  

tagsAll :: OpenApi -> [OaTags]
tagsAll oAPI = map t2tr (oa2t oAPI)   

---- 
-- Info 
data OaInfoRec  = 
  OaInfoRec { 
            oaInfoTitle :: String 
          , oaInfoVersion :: String  
          , oaInfoDescription :: String
          , oaInfoTermsOfService :: String  
          , oaInfoContact :: String
          , oaInfoLicense :: String
            } deriving (Show, Generic) 

defaultoaInfoRec = OaInfoRec "" "" "" "" "" ""

instance C.ToRecord OaInfoRec
instance C.FromRecord OaInfoRec

instance C.FromNamedRecord OaInfoRec
instance C.ToNamedRecord OaInfoRec
instance C.DefaultOrdered OaInfoRec 

oaInfo :: OpenApi -> OaInfoRec 
oaInfo o = 
       OaInfoRec 
            (show (_infoTitle i))  
            (show (_infoVersion i))             
            (show (fromMaybe "" (_infoDescription i))) 
            (show (fromMaybe "" (_infoTermsOfService i)))  
            (show (fromMaybe defaultContact (_infoContact i))) 
            (show (fromMaybe defaultLicense (_infoLicense i))) 
        where i = _openApiInfo o 

----
-- Version 
newtype OaVersion = OaVersion { oaVersion :: String } deriving (Show, Generic) 

defaultOaVersion = OaVersion "" 

instance C.ToRecord OaVersion
instance C.FromRecord OaVersion

instance C.FromNamedRecord OaVersion
instance C.ToNamedRecord OaVersion
instance C.DefaultOrdered OaVersion 

oaVer :: OpenApi -> OaVersion 
oaVer o = OaVersion (show (_openApiOpenapi o)) 

----
-- ExternalDocs 
data OaExternalDocs = 
  OaExternalDocs { 
        oaExternalDocsDescription :: String 
      , oaExternalDocsUrl :: String 
      } deriving (Show, Generic) 

defaultOaExternalDocs = OaExternalDocs "" "" 

instance C.ToRecord OaExternalDocs
instance C.FromRecord OaExternalDocs

instance C.FromNamedRecord OaExternalDocs
instance C.ToNamedRecord OaExternalDocs
instance C.DefaultOrdered OaExternalDocs 

oaExtDocs :: OpenApi -> OaExternalDocs 
oaExtDocs o = 
  OaExternalDocs 
    (show (_externalDocsDescription e)) 
    (show (_externalDocsUrl e)) 
    where 
      e = fromMaybe defaultExternalDocs ( _openApiExternalDocs o) 

----
-- PathItem 
isGet :: PathItem -> Bool
isGet = isJust._pathItemGet 
isPut :: PathItem -> Bool
isPut = isJust._pathItemPut 
isPost :: PathItem -> Bool
isPost = isJust._pathItemPost 
isDelete :: PathItem -> Bool
isDelete = isJust._pathItemDelete 
isOptions :: PathItem -> Bool
isOptions = isJust._pathItemOptions 
isHead :: PathItem -> Bool
isHead = isJust._pathItemHead 
isPatch :: PathItem -> Bool
isPatch = isJust._pathItemPatch 
isTrace :: PathItem -> Bool
isTrace = isJust._pathItemTrace 

isGetText :: PathItem -> Maybe T.Text
isGetText v = if isGet v then Just "Get" else Nothing 
isPutText :: PathItem -> Maybe T.Text
isPutText v = if isPut v then Just "Put" else Nothing 
isPostText :: PathItem -> Maybe T.Text
isPostText v = if isPost v then Just "Post" else Nothing 
isDeleteText :: PathItem -> Maybe T.Text
isDeleteText v = if isDelete v then Just "Delete" else Nothing 
isOptionsText :: PathItem -> Maybe T.Text
isOptionsText v = if isOptions v then Just "Options" else Nothing 
isHeadText :: PathItem -> Maybe T.Text
isHeadText v = if isHead v then Just "Head" else Nothing 
isPatchText :: PathItem -> Maybe T.Text
isPatchText v = if isPatch v then Just "Patch" else Nothing 
isTraceText :: PathItem -> Maybe T.Text
isTraceText v = if isTrace v then Just "Trace" else Nothing 

allOps :: PathItem -> [T.Text]
allOps v 
    = catMaybes [isGetText v, isPutText v, isPostText v, isDeleteText v, 
      isOptionsText v, isHeadText v, isPatchText v, isTraceText v]  

sumOps :: PathItem -> Integer 
sumOps p = sum (map b2i [isGet p, isPut p, isPost p, isDelete p, isOptions p, isHead p, isPatch p, isTrace p]) 

pathSplit :: String -> [String] 
pathSplit = splitOn "/"

data PathItemData  = 
  PathItemData { 
            pathItemKey :: String 
          , pathItemTagName :: String  
          , pathItemOpNames :: String            
          , pathItemIsGet :: String
          , pathItemIsPut :: String  
          , pathItemIsPost :: String  
          , pathItemIsDelete :: String  
          , pathItemIsOptions :: String
          , pathItemIsHead :: String  
          , pathItemIsPatch :: String  
          , pathItemIsTrace :: String 
          , pathItemOpCount :: String 
          , pathItemOpParamNames :: String  
          , pathItemOpParamCount :: String  
          , pathItemKeyLs :: String            
          , pathItemKeyL1 :: String   
          , pathItemKeyL2 :: String             
          , pathItemKeyL3 :: String   
          , pathItemKeyL4 :: String   
          , pathItemKeyL5 :: String   
          , pathItemKeyMoreLs :: String       
          , pathItemKeySummary :: String   
          , pathItemKeyDescription :: String   
          , pathItemKeyServers :: String   
          } deriving (Show, Generic) 

defaultPathItemData :: PathItemData
defaultPathItemData = PathItemData "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" 

instance C.ToRecord PathItemData
instance C.FromRecord PathItemData

instance C.FromNamedRecord PathItemData
instance C.ToNamedRecord PathItemData
instance C.DefaultOrdered PathItemData 

unwrapParam :: Referenced Param -> Param 
unwrapParam (Inline a) = a
unwrapParam (Ref x) = defaultParam 

-- pi2optags _pathItemGet v 
pi2optags :: (PathItem -> Maybe Operation) -> PathItem -> Maybe T.Text 
pi2optags po v 
    | lt == 0 = Nothing 
    | lt == 1 = Just (head tags)  
    | otherwise = Just "!@#error - more than 1 tag! " 
   where 
        tags = IOHS.toList (_operationTags (fromMaybe defaultOperation (po v))) 
        lt = length tags 

pi2optagsAll :: PathItem -> [T.Text] 
pi2optagsAll v 
    =  catMaybes [pi2optags _pathItemGet v, pi2optags _pathItemPut v, pi2optags _pathItemPost v, pi2optags _pathItemDelete v, 
        pi2optags _pathItemOptions v, pi2optags _pathItemHead v, pi2optags _pathItemPatch v, pi2optags _pathItemTrace v] 

-- pi2opparamNames _pathItemGet 
pi2opparamNames :: (PathItem -> Maybe Operation) -> PathItem -> [T.Text] 
pi2opparamNames po v = map (_paramName . unwrapParam) (_operationParameters (fromMaybe defaultOperation (po v))) 

pi2opparamNamesAll :: PathItem -> [T.Text] 
pi2opparamNamesAll v 
    =  pi2opparamNames _pathItemGet v ++ pi2opparamNames _pathItemPut v ++ pi2opparamNames _pathItemPost v 
    ++ pi2opparamNames _pathItemDelete v ++ pi2opparamNames _pathItemOptions v ++ pi2opparamNames _pathItemHead v 
    ++ pi2opparamNames _pathItemPatch v ++ pi2opparamNames _pathItemTrace v 

pi2pid :: (FilePath, PathItem) -> PathItemData 
pi2pid (k,v) = 
       PathItemData 
            (show k)
-- always has at least 1 item, so this is safe 
            (show (head (pi2optagsAll v)))  
            (show (if length ao == 1 then head ao else T.concat ao))   
            (show01 (isGet v))  
            (show01 (isPut v))             
            (show01 (isPost v)) 
            (show01 (isDelete v))  
            (show01 (isOptions v))  
            (show01 (isHead v))             
            (show01 (isPatch v)) 
            (show01 (isTrace v))  
            (show (sumOps v)) 
            (show ns) 
            (show (length ns))   
            (show ll)            
            (show (head ls))
            (show (if ll > 1 then head (tail ls) else "")) 
            (show (if ll > 2 then head (tail (tail ls)) else "")) 
            (show (if ll > 3 then head (tail (tail (tail ls))) else "")) 
            (show (if ll > 4 then head (tail (tail (tail (tail ls)))) else ""))         
            (show (if ll > 5 then 1 else 0))          
            (show (fromMaybe "" (_pathItemSummary v)))  
            (show (fromMaybe "" (_pathItemDescription v)))  
            (show (_pathItemServers v))  
            where 
                ls = tail (pathSplit k) 
                ll = length ls 
                ns = pi2opparamNamesAll v 
                ao = allOps v

pathItemsAll ::  OpenApi -> [PathItemData]  
pathItemsAll oAPI = map pi2pid (fps oAPI)    

----
data PathItemSchema  = 
  PathItemSchema { 
            pathItemSchemaKey :: String 
          , pathItemSchemaName :: String      
          , pathItemSchemaIn :: String                        
          , pathItemSchemaType :: String  
          , pathItemSchemaString :: String  
          , pathItemSchemaTypeInline :: String            
          , pathItemSchemaFormatInline :: String   
          } deriving (Show, Generic) 

defaultPathItemSchema :: PathItemSchema
defaultPathItemSchema = PathItemSchema "" ""  "" ""  "" "" "" 

instance C.ToRecord PathItemSchema
instance C.FromRecord PathItemSchema

instance C.FromNamedRecord PathItemSchema
instance C.ToNamedRecord PathItemSchema
instance C.DefaultOrdered PathItemSchema 

pi2piList :: (PathItem -> Maybe Operation) -> (FilePath, PathItem) -> [(FilePath, Referenced Param)]
pi2piList po (k,v) = map (k,) ( _operationParameters (fromMaybe defaultOperation (po v))) 

pi2piListAll :: (FilePath, PathItem) -> [(FilePath, Referenced Param)]
pi2piListAll (k,v) 
    = pi2piList _pathItemGet (k,v) ++ pi2piList _pathItemPut (k,v) ++ pi2piList _pathItemPost (k,v) 
    ++ pi2piList _pathItemDelete (k,v) ++ pi2piList _pathItemOptions (k,v) ++ pi2piList _pathItemHead (k,v) 
    ++ pi2piList _pathItemPatch (k,v) ++ pi2piList _pathItemTrace (k,v) 

getRefSchemaType :: Maybe (Referenced Schema) -> T.Text 
getRefSchemaType (Just (Inline x)) = "Inline"  
getRefSchemaType (Just (Ref x)) = "Ref" 
getRefSchemaType Nothing = "Nothing" 

unwrapSchemaRef :: Maybe (Referenced Schema) -> T.Text 
unwrapSchemaRef (Just (Ref x)) = getReference x  
unwrapSchemaRef (Just a) = ""
unwrapSchemaRef Nothing = "" 

unwrapRefSchema :: Maybe (Referenced Schema) -> Schema 
unwrapRefSchema (Just (Inline x)) = x  
unwrapRefSchema (Just a) = defaultSchema 
unwrapRefSchema Nothing = defaultSchema 

sc2openapitype :: Schema -> OpenApiType
sc2openapitype v
    | isJust x = fromJust x
    | otherwise = OpenApiNull 
     where x = _schemaType v

sc2Format :: Schema -> T.Text
sc2Format v
    | isJust x = fromJust x
    | otherwise = "" 
     where x = _schemaFormat v

pi2pir :: (FilePath, Referenced Param) -> PathItemSchema 
pi2pir (k,v) = 
       PathItemSchema 
            (show k)
            (show (_paramName p))      
            (show (_paramIn p))                     
            (show (getRefSchemaType ps)) 
            (show (unwrapSchemaRef ps)) 
            (show (sc2openapitype sc)) 
            (show (sc2Format sc)) 
            where 
                p = unwrapParam v 
                ps = _paramSchema p 
                sc = unwrapRefSchema ps 

pathItemSchemaAll :: OpenApi -> [PathItemSchema] 
pathItemSchemaAll oAPI = map pi2pir (concatMap pi2piListAll (fps oAPI)) 

----
-- Operation 
data OaOperation = 
  OaOperation { 
        oaOperationFilePath :: String 
      , oaOperationOpName :: String
      , oaOperationTag :: String 
      , oaOperationParameters :: String 
      , oaOperationResponses :: String 
      , oaOperationRequestBody :: String 
      , oaOperationSummary :: String 
      , oaOperationDescription :: String 
      , oaOperationOperationId :: String 
      , oaOperationDeprecated :: String 
      , oaOperationSecurity :: String 
      , oaOperationServers :: String
      , oaOperationCallbacks :: String 
      , oaOperationExternalDocs :: String 

      } deriving (Show, Generic) 

defaultOaOperation = OaOperation "" "" "" "" "" "" "" "" "" "" "" "" "" "" 

instance C.ToRecord OaOperation
instance C.FromRecord OaOperation

instance C.FromNamedRecord OaOperation
instance C.ToNamedRecord OaOperation
instance C.DefaultOrdered OaOperation 

fps :: OpenApi -> [(FilePath,PathItem)] 
fps = IOHM.toList._openApiPaths 

fp2opGet :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opGet (k,v) = (k, "get", _pathItemGet v) 

fp2opPut :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opPut (k,v) = (k, "put", _pathItemPut v) 

fp2opPost :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opPost (k,v) = (k, "post", _pathItemPost v) 

fp2opDelete :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opDelete (k,v) = (k, "delete", _pathItemDelete v) 

fp2opOptions :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opOptions (k,v) = (k, "options", _pathItemOptions v) 

fp2opHead :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opHead (k,v) = (k, "head", _pathItemHead v) 

fp2opPatch :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opPatch (k,v) = (k, "patch", _pathItemPatch v) 

fp2opTrace :: (FilePath,PathItem) -> (FilePath,T.Text,Maybe Operation)  
fp2opTrace (k,v) = (k, "trace", _pathItemTrace v) 

fp2ops :: (FilePath,PathItem) -> [(FilePath,T.Text,Maybe Operation)] 
fp2ops (k,v) = [fp2opGet (k,v), fp2opPut (k,v), fp2opPost (k,v), fp2opDelete (k,v), 
    fp2opOptions (k,v), fp2opHead (k,v), fp2opPatch (k,v), fp2opTrace (k,v)] 

op2Maybe :: (FilePath,T.Text,Maybe Operation) -> Maybe (FilePath,T.Text,Operation)
op2Maybe (f, t, mo)  
    | isJust mo = Just (f, t, fromJust mo) 
    | otherwise = Nothing 

catOps :: (FilePath,PathItem) -> [(FilePath,T.Text,Operation)] 
catOps (k,v) = mapMaybe op2Maybe (fp2ops (k,v)) 

op2TagNames :: Operation -> [T.Text] 
op2TagNames op = IOHS.toList (_operationTags op) 

op2ParamNames :: Operation -> [T.Text] 
op2ParamNames op = map (_paramName.unwrapParam) (_operationParameters op) 

op2HttpCodes :: Operation -> [Int] 
op2HttpCodes op = IOHM.keys (_responsesResponses (_operationResponses op))  

op2RB :: Operation -> RequestBody 
op2RB op = unwrapRequestBody (fromMaybe defaultRefAny (_operationRequestBody op))  

mrs2ref :: Maybe (Referenced Schema) -> T.Text 
mrs2ref mrs 
    | isNothing mrs = "" 
    | isRef rs = unwrapRef rs 
    | otherwise = "" 
    where 
        rs = fromJust mrs 

mto2ref :: MediaTypeObject -> T.Text 
mto2ref = mrs2ref._mediaTypeObjectSchema 

rb2Medias rrb = map (second mto2ref) (IOHM.toList (_requestBodyContent rrb)) 

fto2op :: (FilePath,T.Text,Operation) -> OaOperation 
fto2op (f, t, o) = 
  OaOperation 
    (show f) 
    (show t) 
    (show (commaText (op2TagNames o)))    
    (show (commaText (op2ParamNames o)))   
    (show (op2HttpCodes o))  
    (show (rb2Medias (op2RB o)))    
    (showBlank (_operationSummary o)) 
    (showBlank (_operationDescription o))
    (showBlank (_operationOperationId o)) 
    (show01 (fromMaybe False (_operationDeprecated o)))  
    (show (_operationSecurity o)) 
    (show (_operationServers o)) 
    (show (_operationCallbacks o)) 
    (showBlank (_operationExternalDocs o))   

operationsAll :: OpenApi -> [OaOperation]
operationsAll oAPI = map fto2op (concatMap catOps (fps oAPI)) 
