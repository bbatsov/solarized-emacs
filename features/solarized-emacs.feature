Feature: Activate solarized theme
  In order to load theme
  As a user
  I want to load theme
  
  Scenario: Load theme
    I load the theme solarized-light
    Then current point should have background color "#fdf6e3"