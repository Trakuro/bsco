import requests
from bs4 import BeautifulSoup

##Functions

##Get this Arcanist Name
#e.g. Shamane, Thirty_Seven

def getArcanistName(soup):
    arcanistName = soup.find("span", {"style":"font-size:95%", "lang":"en"}).text.replace(" ","-")
    return arcanistName 

#It is far more than accurate, but It will work for now.

##Get this Arcanist Stat at Certain Ins/Lvl

def getArcanistStats(soup):
    statTable = soup.find("table",{"class":"ar-stats"})
    return statTable

#And Format it into certain patterns
#e.g

##Main Part

content = requests.get("https://wiki.biligame.com/reverse1999/%E9%AC%83%E6%AF%9B%E6%B2%99%E7%A0%BE").text

soup = BeautifulSoup(content, "html.parser")


print(getArcanistName(soup))
statTable = getArcanistStats(soup)

for tr in statTable.findAll("tr"):
    for td in statTable.findAll("td"):
        print(td)
