# Barcodes

- stored as the string to quich barcode decodes, prefixed by format
  - e.g. EAN13:0123456789111
- either
 - product barcodes, which are global and outside our control
  - or user barcodes, which are UUIDS printed as QR codes

# Food

- id
- short name
- reference image
- density
- nutrients: Nutrient -> Value/100g

# Nutrient

## Known nutrients
## Novel nutrient


# InventoryItem

- id
- barcode
- food id
- purchase date
- image
- storage life
  - current weight
- type:
  - whole food
	- weight
  - sealed container
	- net weight
   - opened container
	 - empty weight

# Inventory

list of Item

## Actions

- add
  - log purchase date
  - log expiration date
  - assign food id
  - log its total weight and net weight
  - assign thumbnail
  - assign barcode

- restock
  - add an unopened container
  - top up an open container

- consume
  - update weight and status

## Queries

- get-containers-by-barcode
- get-containers-by-food-id
- get-containers-by-name
- get-containers-by-expiration

- get-total-quantity-by-barcode
- get-total-quantity-by-food-id
- get-total-quantity-by-name

# Recipe

- id
- original text
- ingredients

## Ingredient

- substitues
  - boolean algebra over Ingredient (x AND y, x OR y)
- types:
  - known
	- food id
	- normalized quantity
  - novel
	- name
	- approx quantity

# Food Journal

- list of (date, time, weight, food id)

# Shopping List

- known ingredients: {name: (quantity, substitutes)}
- novel ingredients: [(name, approx quantity, substitutes)]

## Result

- pending
- available(quantity)
- substituted([(ingredient, quantity)])

# Workflows

## Take Inventory / Resupply

Initial setup / after shopping:

for all items
- scan barcode
- assign container ID
- for new / unknown item
  - take photo
  - enter nutritional info
- weigh item
- for sealed containers, log
  - net wt
  - expiration date
- for open containers / unpackaged items
  - purchase date
  - expected shelf life

## Plan

set nutritional targets for planning period

- choose start and end sates
- set daily nutrient totals
- select recipes to satisfy targets
  - user can scale recipe at this time
- for each novel recipe
 - normalize the recipe

## Normalize Recipes

- for each each ingredient of each novel recipe
  - assign food id
	- for novel ingredient, create placeholder entry
    - for known ingredient, normalize quantity to mass

## Generate Shopping List

for each ingredient of each normalized recipe
  - for a known ingredient
	- scale amount by recipe scale factor
	- adjust required amount as desired by user
	- add calculated amount to required total
  - for a novel ingredient
	- if substituting with known ingredient, goto above
	- else, add ingredient to shopping list

for each known ingredient
- compare required total against inventory total
  - if inventory total <= required total, add to shopping list

## Cook

for given recipe and inventory

### mise en place

for each recipe ingredient
- assign inventory container
  - update inventory
- dispense each ingredient
  - log amount used

### clean
- check in each container, update inventory with current weight
- create food item entry using ingredient totals
  - derive nutritional info
  - set portion size

## Serve

- check out food items to be eaten
- dispense portion of each by weight
- log totals to food journal

## Review

Generate nutrition totals for time given time period

## Snack

Suggest foods and quantities which satisfy nutritional goals
Show the effect of consuming a particular food without modifying the journal

## Archive

Remove dead / orphaned / outdated items and compress DB
