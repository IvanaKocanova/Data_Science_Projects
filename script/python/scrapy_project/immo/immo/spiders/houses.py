import scrapy
from scrapy.crawler import CrawlerProcess


class PriceSpider(scrapy.Spider):
    """Scrape house prices in Bruck and der Leitha"""
    name = 'houses'
    start_urls = ['https://www.immowelt.at/liste/bezirk-bruck-an-der-leitha/haeuser/kaufen?sort=relevanz']


    def parse(self, response):
        for houses in response.css('div.hardfacts_4.clear'):

            price = houses.css('strong::text').get()
            rooms = houses.css('div.hardfact.rooms::text').get()
            size =  houses.css('div.hardfact.square_meters::text').get()

            for item in zip(price, rooms, size):
                scraped_info = {
                    'price': item[0],
                    'rooms': item[1],
                    'size': item[2]
                }
                yield {scraped_info}

        next_page = response.css('a.btn_01.white').attrib['href']
        if next_page is not None:
            yield response.follow(next_page, callback=self.parse)

