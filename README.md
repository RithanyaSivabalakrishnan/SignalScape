# SignalScape
Urban Mobile Network Analysis Dashboard

# Overview
This project analyzes and visualizes mobile network performance using real-world signal data across multiple cities. The goal is to help network planners and analysts understand coverage, quality, and problem areas using interactive analytics.

# Features
~ Interactive heatmaps to display geographic signal strength.

~ Bar charts, box plots, and scatter plots to compare carriers and explore data relationships.

~ K-means clustering to identify natural groupings of network cells and problem zones.

~ Categorical variables for signal strength and quality for easy interpretation.

~ Filtering by city, band, carrier, and signal score.

~ Dashboard built with R Shiny for easy data exploration.

# Analytical Concepts Used
~ Signal strength (RXLEV dBm), SNR, download/upload speed.

~ Feature engineering for categorical scores.

~ Heatmap visualization for spatial trends.

~ Scatter and box plots for metric relationships.

~ K-means clustering for non-linear analysis.

~ Linear modeling (with limitations explained in dashboard).

# Why Not Linear Model?
Linear regression was attempted, but network data showed high variance and did not fit well to a simple linear relationship. K-means clustering provides a better way to identify operational patterns, outliers, and distinct groups without forcing linear assumptions.

# Tools Used
~ R, R Shiny

~ leaflet (mapping)

~ ggplot2 (visualization)

~ dplyr, tidyr (data cleaning)

~ terra, sf (spatial data integration)

~ K-means clustering algorithms

# Data Source
This project uses the following Kaggle dataset:


Network Features Dataset

Author: Alankar Tripathi

Kaggle. https://www.kaggle.com/datasets/alankartripathi/network-features-dataset


Population raster files for spatial enhancement as well.
