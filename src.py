import json
import re
import requests
from bs4 import BeautifulSoup

## Get a List of All Arcanist Chinese Names
# getArcanist :: [Text]
def getArcanistList():
    content = requests.get("https://res1999.huijiwiki.com/wiki/%E8%A7%92%E8%89%B2%E5%88%97%E8%A1%A8").text
    soup = BeautifulSoup(content, "html.parser")
    
    ArcanistList = []
    for charaTab in soup.findAll("div", {"class": "tabber-item no-transition no-count"}):
        for link in charaTab.findAll("a", {"href": True}):
            ArcanistList.append(link.string)

    return ArcanistList

##Return the RLCONF object in Echart, which is a json object.
##It need to merge with the Attribute parsing soon. However, Currently it is:
# parseArcanistAttribute :: Soup a -> Dict a
def parseArcanistAttribute(soup):
    js_code = soup.find("script").string
    pattern = r'RLCONF\s*=\s*({.*?});'
    match = re.search(pattern, js_code, re.DOTALL)
    rlconf_string = match.group(1)
    rlconf_dict = json.loads(rlconf_string)

    Attribute = {}
    for AttrTypeIndex in range(5):
        AttrType = AttrTypeList[AttrTypeIndex]
        Attribute[AttrType]=[]
        for Insight in range(4):
            Attribute[AttrType].append([])
            Attribute[AttrType][Insight].append(0)
            for Level in range([30,40,50,60][Insight]):
                Attribute[AttrType][Insight].append(rlconf_dict["hjEChartsConfig"][""]["option"]["options"][Insight]["series"][AttrTypeIndex]["data"][Level][1])

    return Attribute
### DONE. Sweet!

##Get Resonant Type with given soup
# getResonanceType :: Soup a -> Text ResonanceType
def getResonanceType(soup):
    imgsrc = soup.select("div.resonate-tabber-item:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > img:nth-child(1)")[0]["alt"]
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

##Get the in-game Arcanist English name
##It may contain specific characters like Russian Characters, spaces and dots
##But for now it does not matter as it is stored as Text
# getName :: soup a -> Text a
def getName(soup):
    return soup.select("#firstHeading > h1:nth-child(2) > span:nth-child(1) > span:nth-child(1) > small:nth-child(1)")[0].text

## return a soup object of a URL
# soup :: Text url -> Soup a
def getSoup(url):
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

##Format the specific Attribute.Attrtype to String, calling the getDiff func to calc InsBonus
# formatAttr :: Dict a -> Text a -> Text a
def formatAttr(Attribute, AttrType):
    return f"({getDiff(Attribute[AttrType])}, {Attribute[AttrType][0][1]}, {Attribute[AttrType][0][30]}, {Attribute[AttrType][1][40]}, {Attribute[AttrType][2][50]}, {Attribute[AttrType][3][60]})"

##The final output of this code. Well i suppose its the hardest part
# printInstance :: Dict a -> Text name -> Text resType -> Text typeName -> IO ()
def printInstance(Attribute, Name, ResType, TypeName):
    
##  InsBonus = [Int]
    InsBonus = []
    for AttrType in AttrTypeList:
        InsBonus.append(getDiff(Attribute[AttrType]))

##  Formatted = [Text]
    Formatted = []
    for AttrType in AttrTypeList:
        Formatted.append(formatAttr(Attribute, AttrType))

    with open("Arcanist/"+TypeName+".hs","w") as file:
        file.write( "{-# LANGUAGE DataKinds, TypeFamilies #-}\n")
        file.write(f"module Hsco.Reco.Arcanist.{TypeName} (\n")
        file.write(f"    {TypeName}\n")
        file.write(f") where\n")
        file.write(f"\n")
        file.write(f"import Hsco.Reco.Resonance (ResonanceType(..))\n")
        file.write(f"import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)\n")
        file.write(f"\n")
        file.write(f"data {TypeName}\n")
        file.write(f"\n")
        file.write(f"instance IsArcanist {TypeName} where\n")
        file.write(f"    arcName = \"{Name}\"\n")
        file.write( "    arcPlainData = ArcanistPlainData {\n")
        file.write(f"        atkGen = fromRaw {Formatted[0]},\n")
        file.write(f"        hpGen = fromRaw {Formatted[1]},\n")
        file.write(f"        rdefGen = fromRaw {Formatted[2]},\n")
        file.write(f"        mdefGen = fromRaw {Formatted[3]},\n")
        file.write(f"        critGen = fromRaw {Formatted[4]}\n")
        file.write( "    }\n")
        file.write(f"\n")
        file.write(f"    type ArcResType {TypeName} = {ResType}\n")
    
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
##---------------------------------------------------------------------------------
##Main Part of Code
# main :: IO ()

## Some kind of meta data i will be used
AttrTypeList = ["Attack", "Health", "RealDef", "MentDef", "CritTech"]
typeNameDict = {
    "葛天": "GeTian",
    "曲娘": "JiuNiangZi",
    "伊索尔德": "Isolde",
    "6": "Six",
    "爱兹拉": "EzraTheodore",
    "可燃点": "Spathodea",
    "鬃毛沙砾": "Shamane",
    "伽菈波那": "BlackDwarf",
    "37": "ThirtySeven",
    "马库斯": "Marcus",
    "皮克勒斯": "Pickles",
    "梅兰妮": "Melania",
    "洁西卡": "Changeling",
    "牙仙": "ToothFairy",
    "新巴别塔": "MsNewBabel",
    "温妮弗雷德": "Eternity",
    "远旅": "Voyager",
    "兔毛手袋": "MedicinePocket",
    "泥鯭的士": "AnAnLee",
    "百夫长": "Centurion",
    "星锑": "Regulus",
    "苏芙比": "Sotheby",
    "未锈铠": "AKnight",
    "红弩箭": "Lilya",
    "槲寄生": "DruvisIII",
    "小叶尼塞": "Enisej",
    "和平乌鲁": "Ulu",
    "沙丝绒": "DesertFlannel",
    "坎吉拉": "Kanjira",
    "挖掘艺术": "Diggers",
    "恐怖通": "Horrorpedia",
    "金蜜儿": "Blonney",
    "喀嚓喀嚓": "Click",
    "坦南特": "Tennant",
    "玛蒂尔达": "MatildaBouanich",
    "五色月": "Satsuki",
    "讣告人": "Necrologist",
    "斯奈德": "Schneider",
    "气球派对": "BalloonParty",
    "十四行诗": "Sonetto",
    "帕米埃": "Dikke",
    "柏林以东": "Bkornblume",
    "夏利": "Charlie",
    "婴儿蓝": "BabyBlue",
    "玛丽莲": "Sweetheart",
    "X": "X",
    "埃里克": "Erick",
    "小梅斯梅尔": "MesmerJr",
    "吵闹鬼": "Poltergeist",
    "莫桑女士": "MsMoissan",
    "爱宠": "Rabies",
    "TTT": "TTT",
    "铅玻璃": "Cristallo",
    "APPLe": "APPLe",
    "红斗篷": "Mondlicht",
    "雾行者": "OliverFog",
    "狼群": "Pavia",
    "芭妮芭妮": "BunnyBunny",
    "冬": "Zima",
    "小春雀儿": "Eagle",
    "尼克·波顿": "NickBottom",
    "斯普特尼克": "Sputnik",
    "洋葱头": "Onion",
    "哒哒达利": "DarleyClatter",
    "贝蒂": "Bette",
    "丽莎&路易斯": "TwinSleep",
    "约翰·提托": "JohnTitor",
    "莉拉妮": "Leilani",
    "星之眼": "AlienT",
    "拉拉泉": "LaSource",
    "弄臣": "TheFool",
    "门": "Door",
    "无线电小姐": "MsRadio"
        }



##Now, Get shamane's soup name restype attribute
##and define its typeName = Shamane

# url = "https://res1999.huijiwiki.com/wiki/%E9%AC%83%E6%AF%9B%E6%B2%99%E7%A0%BE"
# soup = getSoup(url)
# name = getName(soup)
# Attribute = parseArcanistAttribute(soup) # a dict actually now
# resType = getResonanceType(soup)
# 
# printInstance(Attribute, name, resType, "Shamane")

ArcanistList = getArcanistList()
for Arcanist in ArcanistList:
    soup = getSoup("https://res1999.huijiwiki.com/wiki/"+Arcanist)
    name = getName(soup)
    resType = getResonanceType(soup)
    Attribute = parseArcanistAttribute(soup)

    printInstance(Attribute,name,resType,typeNameDict[Arcanist])

