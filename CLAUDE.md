# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository is for managing a La Presse hockey pool (https://www.marqueur.com/hockey/mbr/games/masterpool/info_03.php). The project consists of multiple modeling and workflow components to support data-driven decision making in the pool.

## Project Architecture

The project is organized into three main modeling components:

### 1. Data Collection Pipeline
- Collect player point projections for the season
- Collect cap hit information for each player
- Store and manage player data for analysis

### 2. Initial Draft Optimization Model
- Optimize initial player selection strategies
- Balance between high-cost elite players and low-cost young players (e.g., players at $900k)
- Evaluate trade-offs between different roster construction strategies

### 3. Dynamic Player Valuation Model
- Implement Bayesian modeling approach using player priors (initial projections as plausible point distributions)
- Incorporate current season performance streaks
- Generate buy low/sell high recommendations based on:
  - Prior expectations (pre-season projections)
  - Current performance relative to expectations
  - Statistical likelihood of regression to mean

## Technology Stack

This project uses LaTeX for documentation and analysis (as indicated by .gitignore configuration).

## Development Notes

- The repository is set up with LaTeX-specific gitignore rules
- Primary language: French (Quebec)
- Pool platform: Marqueur.com La Presse Masterpool
