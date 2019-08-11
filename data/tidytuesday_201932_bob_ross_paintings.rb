#!/usr/bin/env ruby
# frozen_string_literal: true

require 'mechanize'
require 'csv'

agent = Mechanize.new
base_url = 'https://www.twoinchbrush.com'
seasons = 1.upto(31)

CSV.open('tidytuesday_201932_bob_ross_paintings.csv', 'wb') do |csv|
  seasons.each do |season|
    index_url = File.join(base_url, 'season', season.to_s)
    index_page = agent.get(index_url.to_s)

    index_page.search('.painting-holder a').each do |link|
      painting_url = File.join(base_url, link['href'])
      painting_page = agent.get(painting_url.to_s)

      title = painting_page.search('h1').first.text

      episode = painting_page.search('.col-12 span').first.text
      episode = episode.gsub("'The Joy of Painting'", '').strip

      puts [episode, title].join(': ')

      painting_page.search('.color-list li a').each do |el|
        color = el.children.first.attribute('style').to_s
        color = color.gsub('color: ', '')
        color = color.gsub(';', '')

        color_name = el.text.strip

        csv << [episode, title, color, color_name]
      end
    end
  end
end
