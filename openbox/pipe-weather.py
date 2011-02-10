#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys
import urllib.request, urllib.parse, urllib.error
from xml.sax import handler, parseString

class ElementProcesser(handler.ContentHandler):
   
    def startElement(self, name, attrs):

        if name == "city":
            print("<separator label='" + attrs["data"] + "' />")
        if name == "current_conditions":
            print("<separator label='Текущая погода' />")
        if name == "condition":
            print("<item label='Погода: " + attrs["data"].encode('cp1251') + "' />")
        if name == "humidity":
            print("<item label='" + attrs["data"] + "' />")
        if name == "wind_condition":
            print("<item label='" + attrs["data"] + "' />")
        if name == "day_of_week":
            print("<separator label='" + self.getDayOfWeek(attrs["data"]) + "' />")
        if name == "temp_c":
            print("<item label='Температура " + attrs["data"].encode('cp1251') + " C' />")
        if name == "low":
            print("<item label='Мин: " + str(self.f2c(attrs["data"])).encode('cp1251') + " C' />")
        if name == "high":
            print("<item label='Макс: " + str(self.f2c(attrs["data"])).encode('cp1251') + " C' />")

    def endElement(self, name):
        if name == "current_conditions":
            print("<separator label='Прогноз' />")
   
    def startDocument(self):
        print('<openbox_pipe_menu>')
   
    def endDocument(self):
        print('</openbox_pipe_menu>')
   
    def getDayOfWeek(self,day):
        day_mass = {"Mon":"Понедельник", "Tue":"Вторник", "Wed":"Среда", "Thu":"Четверг", "Fri":"Пятница", "Sat":"Суббота", "Sun":"Воскресенье"}
        return day_mass[day]
           
    def f2c(self,farengeit):
	    return  (int(farengeit) - 32) * 5/9

# You should use your local version of google to have the messages in your language and metric system
f = urllib.request.urlopen("http://www.google.com/ig/api?weather="+sys.argv[1])
xml = f.read()
f.close()


parseString(xml,ElementProcesser())
