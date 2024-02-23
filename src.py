import json
import re
import requests
from bs4 import BeautifulSoup

def getArcanistList():
    content = requests.get("https://res1999.huijiwiki.com/wiki/%E8%A7%92%E8%89%B2%E5%88%97%E8%A1%A8").text
    soup = BeautifulSoup(content, "html.parser")
    
    ArcanistList = []
    for charaTab in soup.findAll("div", {"class": "tabber-item no-transition no-count"}):
        for link in charaTab.findAll("a", {"href": True}):
            ArcanistList.append(link.string)

    return ArcanistList

def parseArcanistAttribute(soup):
    js_code = soup.find("script").string

    pattern = r'RLCONF\s*=\s*({.*?});'
    match = re.search(pattern, js_code, re.DOTALL)

    if match:
        rlconf_string = match.group(1)
        rlconf_dict = json.loads(rlconf_string)
        return rlconf_dict
    else:
        print("RLCONF not found in the JavaScript code.")

def formatAttr(Attribute, AttrType):
    print(f"({getDiff(Attribute[AttrType])}, {Attribute[AttrType][0][1]}, {Attribute[AttrType][0][30]}, {Attribute[AttrType][1][40]}, {Attribute[AttrType][2][50]}, {Attribute[AttrType][3][60]})")

def getResonanceType(soup):
    imgsrc = soup.select("div.resonate-tabber-item:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > img:nth-child(1)")[0]["alt"]
#   print(imgsrc)
    match imgsrc:
        case "Fw 011.png":
            return "TypeZ"

        case "Fw 013.png":
            return "TypeT"

        case "Fw 010.png":
            return "TypeX"

        case "Fw 014.png":
            return "TypeU"

        case _:
            return None


def getName(soup):
    return soup.select("#firstHeading > h1:nth-child(2) > span:nth-child(1) > span:nth-child(1) > small:nth-child(1)")[0].text

# #firstHeading > h1:nth-child(2) > span:nth-child(1) > span:nth-child(1) > small:nth-child(1)

# return a soup object::
def soup(url):
    content = requests.get(url).text
    soup = BeautifulSoup(content, "html.parser")
    return soup

# Porting from Syl's Code
def interpolate(x1, y1, x2, y2, x):
    return int((y2 - y1) / (x2 - x1) * (x - x1) + y1)

def checkDiff(data, diff):
    for k in range(0, 30):
        if interpolate(1, data[0][1], 30, data[0][30], k + 1) != data[0][k + 1]:
            return False
    for k in range(0, 40):
        if interpolate(0, data[0][30] + diff, 40, data[1][40], k + 1) != data[1][k + 1]:
            return False
    for k in range(0, 50):
        if interpolate(0, data[1][40] + diff, 50, data[2][50], k + 1) != data[2][k + 1]:
            return False
    for k in range(0, 60):
        if interpolate(0, data[2][50] + diff, 60, data[3][60], k + 1) != data[3][k + 1]:
            return False
    return True

def getDiff(data):
    for diff in range(10000):
        if checkDiff(data, diff):
            return diff
    return -1

def printInstance(Attribute, Name, ResType, TypeName):
    print("{-# LANGUAGE DataKinds, TypeFamilies #-}")
    print(""" module Hsco.Reco.Arcanist.{TypeName} (
    {TypeName}
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data {TypeName}

instance IsArcanist {TypeName} where
    arcName = "{Name}"
    arcPlainData = ArcanistPlainData \{
        atkGen = fromRaw {formatAttr(Attributes, "Attack")},
        hpGen = fromRaw {formatAttr(Attributes, "Health")},
        rdefGen = fromRaw {formatAttr(Attributes, "RealDef")},
        mdefGen = fromRaw {formatAttr(Attributes, "MentDef")},
        critGen = fromRaw {formatAttr(Attributes, "CritTech")}
    \}

    type ArcResType {TypeName} = TypeName""").format()
    
# {-# LANGUAGE DataKinds, TypeFamilies #-}
# module Hsco.Reco.Arcanist.ThirtySeven (
#     ThirtySeven
# ) where
# 
# import Hsco.Reco.Resonance (ResonanceType(..))
# import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)
# 
# data ThirtySeven
# 
# instance IsArcanist ThirtySeven where
#     arcName = "Thirty-seven"
#     arcPlainData = ArcanistPlainData {
#         atkGen = fromRaw (36, 269, 408, 683, 1019, 1199),
#         hpGen = fromRaw (166, 1242, 1885, 3160, 4712, 5543),
#         rdefGen = fromRaw (15, 113, 172, 288, 430, 505),
#         mdefGen = fromRaw (17, 131, 198, 332, 495, 582),
#         critGen = fromRaw (41, 313, 313, 354, 395, 436)
#     }
# 
#     type ArcResType ThirtySeven = TypeX

# TypeX TypeZ TypeU TypeT
# main :: IO ()
# span style="display: flex;flex-direction: column;"

url = "https://res1999.huijiwiki.com/wiki/%E9%AC%83%E6%AF%9B%E6%B2%99%E7%A0%BE"
soup = soup(url)
name = getName(soup)
AttrSoup = parseArcanistAttribute(soup)
resType = getResonanceType(soup)

#Attribute :: (Text AttrType, Int Insight, Int Level, [Int] Data) => AttrType -> Insight -> Level -> Data

AttrTypeList = ["Attack", "Health", "RealDef", "MentDef", "CritTech"]
Attribute = {}
for AttrTypeIndex in range(5):
    AttrType = AttrTypeList[AttrTypeIndex]
    Attribute[AttrType]=[]
    for Insight in range(4):
        Attribute[AttrType].append([])
        Attribute[AttrType][Insight].append(0)
        for Level in range([30,40,50,60][Insight]):
            Attribute[AttrType][Insight].append(AttrSoup["hjEChartsConfig"][""]["option"]["options"][Insight]["series"][AttrTypeIndex]["data"][Level][1])
# print(Attribute["Attack"][0][30])

AttrInsBonus = {}
for AttrType in AttrTypeList:
    AttrInsBonus[AttrType] = getDiff(Attribute[AttrType])

# data StatGenerator = StatGenerator {
#     insight :: Int, -- insight bonus
#     stat01 :: Int, -- stat at insight 0 level 1
#     stat030 :: Int,
#     stat140 :: Int,
#     stat250 :: Int,
#     stat360 :: Int
# }
   
printInstance(Attribute, name, resType, "Shamane")
