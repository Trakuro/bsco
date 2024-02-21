import requests
from bs4 import BeautifulSoup

##Functions

##Get this Arcanist Name
#e.g. Shamane, Thirty_Seven

def getArcanistName(soup):
    arcanistName = soup.find("span", {"style":"font-size:95%", "lang":"en"}).text
    return arcanistName 

#It is far more than accurate, but It will work for now.

##Get this Arcanist Stat at Certain Ins/Lvl

def getArcanistStats(soup):
    statTable = soup.find("table",{"class":"ar-stats"})
    return statTable

#And Format it into certain patterns
#e.g

# instance IsArcanist ThirtySeven where
#     arcName = "Thirty-seven"
#     arcPlainData = ArcanistPlainData {
#         atkGen = fromRaw (36, 269, 408, 683, 1019, 1199),
#         hpGen = fromRaw (166, 1242, 1885, 3160, 4712, 5543),
#         rdefGen = fromRaw (15, 113, 172, 288, 430, 505),
#         mdefGen = fromRaw (17, 131, 198, 332, 495, 582),
#         critGen = fromRaw (41, 313, 313, 354, 395, 436)
#     }

##Main Part

content = requests.get("https://wiki.biligame.com/reverse1999/%E9%AC%83%E6%AF%9B%E6%B2%99%E7%A0%BE").text

soup = BeautifulSoup(content, "html.parser")


print(getArcanistName(soup))
statTable = getArcanistStats(soup)

f = open("thisTable.md","w")
f.write(statTable.prettify())
f.close()


for tr in statTable.findAll("tr"):
#   for th in tr.findAll("th"):
#       print(th.getText())

    for td in tr.findAll("td"):
        print(td.getText())
