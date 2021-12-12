#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import scrapy

class PriceSpider(scrapy.Spider):
    name = 'price'
    start_urls = ['https://www.immowelt.at/liste/bezirk-bruck-an-der-leitha/haeuser/kaufen?sort=relevanz']
    
    def parse(self, response):
        for houses in response.css('div.hardfacts_4.clear') :
            yield {
                'price': houses.css('strong::text').get()  
                'rooms': houses.css('div.hardfact.rooms::text').get()     
                'size': houses.css('div.hardfact.square_meters::text').get()  
            }
        

