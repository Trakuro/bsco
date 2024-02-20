import requests
from bs4 import BeautifulSoup

content = requests.get("https://wiki.biligame.com/reverse1999/%E9%AC%83%E6%AF%9B%E6%B2%99%E7%A0%BE").text

soup = BeautifulSoup(content, "html.parser")

# print(soup.prettify())

for item in soup.findAll("table",{"class":"ar-stats"}):
    print(item)

