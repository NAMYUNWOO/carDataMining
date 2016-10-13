import scrapy
from selenium import webdriver
from scrapy.selector import Selector
import time
import csv 

from carmessages.items import CarmessagesItem
matrix = []
f = open("/Users/yunwoonam/Desktop/cartalks/cartalk5.csv",'rb')
csvReader = csv.reader(f)
for row in csvReader:
    matrix.append(str(row)[2:-2])
f.close()
del matrix[0]

class EPLSpider(scrapy.Spider):
    name = "carComment5"
    allowed_domains = ["auto.naver.com"]
    start_urls = matrix
    
    def __init__(self):
        scrapy.Spider.__init__(self)
        self.browser = webdriver.Chrome("/Users/yunwoonam/Desktop/python/chromedriver/chromedriver")
        


    def parse(self, response):
 
        self.browser.get(response.url)
        time.sleep(1)

        html = self.browser.find_element_by_xpath('//*').get_attribute('outerHTML')
        selector = Selector(text=html)
        model = selector.xpath('//*[@id="container"]/div[1]/div/h3')
        modelex = model.extract()
        modelex0 = modelex[0].encode('utf-8')
        firstXpath = ['//*[@id="comment_box_module"]/div/div[5]/ul/li[1]/div[1]/div/div[2]/span[2]','//*[@id="comment_box_module"]/div/div[5]/ul/li[1]/div[1]/div/div[2]/span']
        try:
            comment = selector.xpath(firstXpath[0])
            commentex = comment.extract()
            commentex0 = commentex[0].encode('utf-8')
        except:
            try:
                comment = selector.xpath(firstXpath[1])
                commentex = comment.extract()
                commentex0 = commentex[0].encode('utf-8')
            except:
                try:
                    comment = selector.xpath('//*[@id="comment_box_module"]/div/div[5]/ul/li/div[1]/div/div[2]/span')
                    commentex = comment.extract()
                    commentex0 = commentex[0].encode('utf-8')
                except:
                    commentex0 = "null"
        
         
        for cn in range(2,14):
            cmxpath = ('//*[@id="comment_box_module"]/div/div[5]/ul/li[%d]/div[1]/div/div[2]/span[2]' %cn)
            cmxpath2 = ('//*[@id="comment_box_module"]/div/div[5]/ul/li[%d]/div[1]/div/div[2]/span' %cn)
            xpaths = [cmxpath,cmxpath2]
            for i in xpaths:
                try:
                    comment = selector.xpath(i)
                    commentex = comment.extract()
                    commentex0 += commentex[0].encode('utf-8')
                except:
                    pass

                
        pn = 1
        under10 = True
        
        while True:
            
            nextXpath = ('//*[@id="comment_box_module"]/div/div[6]/div/a[%d]'%pn)
            
            if pn is 1:
                pn += 2
            else:
                pn += 1
            try:
                next = self.browser.find_element_by_xpath(nextXpath)
            except:
                break
            if pn is 12 and under10 is True:
                pn = 3
                under10 = False
            if pn is 13 and under10 is False:
                pn = 3
                
            try:
                next.click()
         
                html = self.browser.find_element_by_xpath('//*').get_attribute('outerHTML')
                selector = Selector(text=html)
                for cn in range(1,11):
                    cmxpath1 = ('//*[@id="comment_box_module"]/div/div[5]/ul/li[%d]/div[1]/div/div[2]/span' %cn)
                    cmxpath2 = ('//*[@id="comment_box_module"]/div/div[5]/ul/li[%d]/div[1]/div/div[1]/span' %cn)
                    cmxpath3 = '//*[@id="comment_box_module"]/div/div[5]/ul/li/div[1]/div/div[2]/span'
                    try:
                        comment = selector.xpath(cmxpath1)
                        commentex = comment.extract()
                        commentex0 += commentex[0].encode('utf-8')
                    except:
                        try:
                            comment = selector.xpath(cmxpath2)
                            commentex = comment.extract()
                            commentex0 += commentex[0].encode('utf-8')
                        except:
                            if cn is 1:
                                comment = selector.xpath(cmxpath3)
                                commentex = comment.extract()
                                commentex0 += commentex[0].encode('utf-8')
                                break
                            else:
                                pass
            except:
                 break
        
        item = CarmessagesItem()
        item['model'] = modelex0
        item['message'] = commentex0
        yield item
        
        
        

