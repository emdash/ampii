# Overview

AMPiI has one fundamental goal: to help you track every gram of food
that goes into your mouth. Internally, AMPiI tracks food *by weight*.

This isn't perfect; however -- and this is important to understand --
perfection is *not* the goal. The goal is to help you do the best you
can at any given time.

Broadly speaking, your time with AMPiI will be spent in the following
ways:
- Set up your Inventory, a one-time task.
- Import recipes, at any time.
- Create a meal plan, from specific recipes: periodically.
- Generate a Shopping List from a meal plan, periodically.
- Update your Inventory, after a shopping trip: as needed.
- Cook dish(es) from your meal plan, as needed.
- Log what you've eaten, as needed.
- Review your progress, at the end of a planning period.

AMPiI is especially oriented towards those who want to prepare food in
advance, a.k.a. *meal prepping*. But it works just fine for those
those meals you prepare just before eating.

AMPiI tracks all the food you eat, whether it's pre-packaged, whole,
or hot from the kitchen.

# System Requirements

## Getting Started

AMPiI's minimum requirements are modest: a compatible web browser. You
can get started using nothing but your smart phone or tablet, if you
insist on it.

But a physical keyboard is *highly* recommended, and ideally one with
a num-pad, as you're going to be typing in a lot of digits.

A camera of some sort is also advisable.

Better still: a desktop or laptop with availabe USB ports, and / or
bluetooth. This will allow you to connect any number of barcode
scanning devices, which will greatly improve the experience.

Barcode scanners generally work like a keyboard. They just "type" in
the characters they decode. You can usually configure these to add a
custom prefix and / or suffix, which is highly recommended. You can
then configure AMPiI to recognize this pattern, which helps to ensure
that scanning a barcode is not mistaken for other arbitrary
keypresses.

Better still: your a computer that can run Linux, or another
open-source operating system, such as FreeBSD. This will allow you to
add a USB scale, and save you from having to type in weights
manually. Unlike barcode scanners, scales use USB at a lower level. At
this time, I am only able to support for usb scales on systems that
provide a `hidraw` device file interface.

## The End Goal: Your Kitchen Companion

You're going to be using AMPiI with dirty hands, in a place where
spills and splatters are likely, so keep that in mind when choosing
your system and its location. How you mitigate these hazards is also
up to you. But you need easy access to both the scale, and the scanner.

AMPiI embraces a bit of a social experiment: the return of the
*kitchen computer*. I'm going to recommend that you eventually
dedicate a computer to AMPiI, that lives in your kitchen.

To this computer, you attach can your kitchen peripherals:
- your digital scale,
- your barcode scanner,
- your camera,
- your barcode label printer.

You're going to need to frequently interacting with AMPiI while in the
kitchen, and this makes little sense if your computer is located in
another room.

How you go about this is up to you. Just remember that AMPiI's
performance requirements are modest. The main thing it needs is a
keyboard, USB ports, and a screen. A Raspberry PI 400 would be
ideal. So long as it can run Linux. Place the screen and input devices
wherever works for you. Add a keyboard cover to protect from spills,
or use a waterproof membrane keybord. Have fun with it. Make it an
expression of who you are.

This computer does not need to be connected to the internet! AMPiI can
work entirely offline. However, if you do have internet access, it
allows the following:

- Most important: keeping your computer's clock set correctly. If you
  choose to run an offline machine, *make sure your clock is set
  correctly*. This is very important, as AMPiI makes extensive use of
  the system time, to save you the hassle of having to manually track
  the date and time of important events.
- Synchronizing your data: this allows you to do planning and tracking
  on other systems, according to your needs.
- Participating in the community food database.
- Receiving software updates to AMPiI, as they become available.

# Nutrition #

In nutrition, *nutrient density* is the amount of some nutrient for
some quantity of food. It's a simple ratio of mass to mass, often
expressed in units like "grams per 100 grams".

AMPiI normalizes all these quantities for foodstuffs, so that totals
can be calculated for any portion of a given food.

# Inventory

The inventory consists of two parts: a food database, with nutritional
information about each food, and a *pantry* which tracks individual
containers of a given food.

Your first task after installation will be to take some
inventory. This is, admittedly, a bit painful. But well worth
doing. You don't have to do it all at once, but the more complete your
inventory is, the better AMPiI will be at tracking everything else
that matters to you.

The key thing to understand is that AMPiI tracks individual
*containers* of food. If you have 10 cans of the same lot of baked
beans, then that corresponds to 10 inventory items. Each container has
a globally unique ID, regardless of the food it contains.

## Adding Inventory Items

In the beginning, you'll do this for everything you already have in
your pantry. You can do it all at once, or gradually. It's up to you.

- identify the food:
  - by searching the food database, or
  - by scanning a product UPC, or
  - by creating a new food item.
- update nutritional facts:
  - make sure the values reflect what's on the package, even if it's
	one that you've seen before.
	- you can tweak nutrition facts for different *lots* of the product.
- assign a container id:
  - by entering manually, or
  - by scanning a unique barcode, or
  - by generating a new container id.
	- be sure to record this on the container.
- weigh the container:
  - by entering the total weight of a *sealed* container, or
  - by updating the total weight of a reusable container.
- enter the net weight or volume:
  - using the last recorded value (the default), or
  - by entering the digits manually.
- enter the expiration, use-by, or best-by date:
  - directly, or
  - calcuated from shelf-life and the a given date (defaulting to the
    current date).
- assign a photo.
  - by searching the database, or
  - by taking a new photo, or
  - by choosing a specific photo file or URL.

## Consumption

As you eat and prepare food, AMPiI will track the consumption.

You can also manually edit the status of any container in your
inventory, should the need arise.

## Retiring Container IDs

Container IDs must be unique, but *retires* the container ID after the
associated container has been consumed.

This allows you to re-use a fixed set of:
- pre-printed barcodes,
- barcoded or numbered containers,
- re-usable barcode tags.

It's up to you if you want to take advantage of this feature.

The key thing is that, at any given time, no two containers can have
the same ID. Once a container is consumes, its ID is released for
future use.

## Maintenance

# Recipes

Every dish starts with a recipe. Well, maybe not every dish. But it at
least starts with a list of ingredients.

How you get your recipes is totally up to you. Online. Print. Your
grandma's hand-written family secrets. It doesn't matter, but you need
to get them into AMPiI.  This allows AMPiI to:

- Calculate the nutritional summary for each dish.
- Generate shopping lists for the recipes you want to make.
- Help you measure each ingredient when cooking.

You don't have to type in the whole recipe. AMPiI only cares about the
ingredients, and amounts. You're free to be as terse or as detailed
with other aspects of your recipe. I do suggest you take the time to
add a photo, as this makes searching recipes much more fun.

AMPiI wants to know *precisely* which food from your database you're
using in your recipe. This is a level of precision that can only be
achieved from within AMPiI, for obvious reasons. So for each
ingredient, you'll be asked to identify the food (either by searching,
or by scanning), and to input a *precise* weight.

As stated above, AMPiI tracks food by weight. Obviously, many recipes
are given in *volumetric* units, especially where I live, in North
America.

Not to worry: AMPiI will help you convert to more scientific
weight-based measurements on the fly. If the density of the food is
known, AMPiI will estimate the weight for you. If it's not, you can
measure by volume when you cook, and the weight will be read off the
scale. Just keep in mind that until a *precise* weight is known,
nutritional totals will be incomplete.

As with the inventory setup, in the beginning, you'll have a lot of
data to enter; over time, you'll build up a library of favorites, and
things will go more smoothly.

With complex recipes, you can group related ingredients into
*bowls*. This helps with measuring ingredients during the cooking
process.

## Refining Recipes from a Batch

You can use the yield and ingredient totals from a previous batch to
refine a quantities a recipe. Not only will this will help achieve
more consistent results, but it will help improve the accuracy of
nutrition tracking.

We haven't talked about batches yet, so don't worry if this doesn't
make sense.

## Recipe Cleaner

There is a browser plugin, [Recipe
Cleaner](https://github.com/erik/recipecleaner) that you may find
helpful when dealing with recipes found on the web.

I'm working separately on Recipe Cleaner to ease integration with AMPiI.

## Substitutions

Sometimes an ingredient can be substituted with one or more
alternatives. AMPiI lets you capture this information precisely, to
make your shopping trips and cooking as painless as possible.

# Planning

Once you've got your inventory set up, and have the recipes you want
to make imported, it's time to make a meal plan.

AMPiI will show you any containers that are nearing or past their
expiration date, and any containers that are getting close to empty.

At minimum, a meal plan is a set of recipes. Each recipe can be scaled
up or down, to reflect the amount of each dish you need to make. From
here, you can generate a shopping list, using the Inventory to filter
out ingredients you already have enough of, but making sure not to
leave out any key ingredients that you don't have.

If you like, you can go further, and create *menus* for a set of
*dates*. Each menu contains some set of *meals*. You complete your
menu by assigning *portions* from recipes to each *meal*. AMPiI can
then scale your recipes for you, so that you make enough to satisfy
your plan.

You can go further still, and establish *goals* for your plan. These
are *constraints* on specific nutrients. For example: no more than
2000 calories per day, at least 30 grams of fiber per day, and no more
than 2300mg of sodium per day. What, how much, or how little, is
entirely up to you, but defaults to established guidelines.

AMPiI will warn you if your plan fails to satisfy these constraints,
allowing you to adjust manually. Also included is a constraint solver,
which can try to figure this out for you, or -- more importantly --
warn you if this is impossible.

The final piece to add to your plan are your *biometric* goals. This
is used during weigh-in, and is are discussed in more detail in the
chapter on review.

# Shopping

You can export your shopping list in a variety of formats. This is a
fertile area for contributors to explore. A variety of integrations
could be possible here.

The most basic format is a web page you can print or save to your mobile device.

Returning from your shopping trip, be sure to update your Inventory
with the items you've purchased before you put them away.

# Cooking

When it's time to cook, choose a set of dishes from your plan, and
enter cooking mode.

This is where AMPiI really begins to shine. But this is also where I
must ask you to change the way you cook somewhat.

You've done all this work up front to set up your inventory, import
your recipes, create meal plan, done your shopping. For this all to
pay off now, you must measure *all* your ingredients *by weight*.
Don't worry, AMPiI will make this easy.

\* *With some common-sense exceptions, like water in a soup or stew.*

## Mise en Place

Now you're going to weigh out your ingredients -- according to your
meal plan, not your original recipe.

If you took the time to group your ingredients into *bowls*, it will
pay off now.

The bowls to be prepared are shown. Adjust the groupings now, if
necessary.

- Place an empty bowl onto the scale.
- Select the bowl to weigh into
- Scan or enter a container id for any of the ingredients to be measured.
- Weigh out the portion according to the calculated amount. You don't need
  to be perfect, just do your best.
- Repeat the last two steps for the rest of the ingredients in the bowl.

When all the bowls are prepared out, you can move on to the next mode.

Where possible, set your containers aside rather than putting away
directly. This will save you time later. If it helps, use a box or tote
to keep them out of the way. Obviously, return perishable items to
cold storage as soon as possible, and keep andy anything you might need
more of.

## Improvise

This part is up to you. Go wild. Or don't.  If you don't have a recipe
selected, this is where cooking mode starts.

First, you need to scan and weigh each container that you dispense from,
but *before* you dispense from it.

No cheating!

How are you going to know how much salt or fat your dish contains, if you
aren't honest about how much you've *really* added? The same would
go for any other ingredient. Many condiments and seasonings are hidden
sources of substances that you care about tracking.

If you're cooking multiple dishes at once, make sure you *log to the
correct recipe.* as you dispense each ingredient. AMPiI can't help you
if you make a mistake here!

In fact, I encourage you to make one dish at a time, until you get a
feel for AMPiI's weight-based approach to cooking, though you're free
to prep for multiple dishes in advance. This way, you only have to
weigh your containers one final time, when the dish is finished.

### Log Batches

Each time you make a dish, AMPiI creates a new *batch*. This is to
reflect the fact that no two batches are every completely identical.

A *batch* appears as a distinct food in your inventory.

The batch retains references to your *mealplan* if you're using one,
as well as the original recipe.

You can assign a container ID for any planned or unplanned leftovers
at this time, to ease tracking later.

You should also, ideally, weigh the final amount, to determine the
yield. If you can't do this easily, just give it your best guess.

The yield is used to calculate the nutrient densities for the
batch. And this, in turn, is used during portioning.

When all your batches are logged, you can proceed to the next mode.

## Check-in

Cooking can be an energetic and chaotic activity. You cannot simply
rely on what you measured out during cooking to update your
inventory. What if you spill? What if you have to start over?

You should ideally weigh each container before you put it away.

If a container has been entirely consumed, you can mark it as such and
skip the weighing step. This will also release the container ID for
reuse.

If you skip doing this, your inventory will still reflect the last
recorded weight for any given container.

Each container you used will remain marked as "in use" until you check
it in.

# Tracking

If you have made a meal plan, you'll see it displayed, with the
current or upcoming meal slot selected in tracking mode.

You can select any meal in your plan, and the details will be
displayed.

You can log meals that were eaten *as planned* with the *log as
planned* command.

Or, you can enter food portions manually.

## Portion Mode

AMPiI can help you portion food according to your meal plan.

Choose the desired meal slot (the next upcoming slot is the default).

Upon entering portion mode, AMPiI will prompt you to place your empty
container on the scale.

- Put your plate, bowl, or other empty container on the scale.
- Scan the indicated container: AMPiI tares the scale.
- Add food from the container, until the portion is as desired. You
  don't need to be perfect, but AMPiI's feedback will make it easy to
  be accurate.
- Scan the next container. AMPiI will tare the weight, and the process repeats.
- AMPiI will let you know when all the items on the menu have been portioned.
- You can, however, keep going. For example, Maybe there's a sauce you
  want to add. It might be a problem for your diet, but it's not a
  problem for AMPiI. Be thorough. Be honest.

When you're finished, you have two choices:
- commit the record directly to your food log, or
- amend your *meaplpan* with the prepared portion.
  - you can later log the meal *as planned* (described above).

## Snack Mode

Ampi will show any containers which are nearing their expiration date.

If you have a meal plan, AMPiI will remind you of your daily totals.

- place an empty container on the scale.
- scan or enter a container id
- dispense a small amount of food from the container

AMPiI will show you the nutritional totals for that portion. If you
have a meal plan, AMPiI will also estimate the effect of this portion
on your nutritional goals, based on the amount food on the scale. It
will subtly nag you if you're about to blow it. Adjust the portion
until you're happy with it.

When you're finished, commit the portion to your log.

# Weigh-In

As part of your plan, you may have specified biometric goals to be
tracked. This goes by the shorthand "weigh-in". But it need not be
limited to, or even involve your weight.

Track whatever means most to you: your BMI, your body fat percentage,
your cholesterol, your blood pressure, your vitamin C, or even your
mood.

However often you tell AMPiI you want to track this information is
how often AMPiI will nudge you to record it.

Be honest. After all, you'd only be only cheating yourself.

# Review Mode

When you've reached the end of your meal plan, AMPiI will nag you to
review. Pick a time when you can focus on it, and enter review mode.

You'll see your "accuracy" score, which is AMPiIs measure of how
complete and accurate your tracking has been. There is a always
uncertainty with tracking food. Perfection is not the goal.

You'll see your biometric goals plotted against your reported figures.

You'll see your nutritional goals, plotted against your recorded daily
totals.

This is your time to reflect on your results. You should ask yourself
the following questions:

- How well am I capturing my data into AMPiI?
- How well am I sticking to my plan?
- Am I on track with my biometric goals?
- Do my nutritional goals need to change?
- Am I eating the right foods?

When you've answered these questions, you are ready to create your
next plan.

# Conclusion

Thanks for sticking with me.

AMPiI may not be to everyone's liking. That's fine with me. It's
open-source. You are free to modify the source code, per the terms of
the AGPL.

If you use it, and like it, please let me know.

If you manage to build a service around it, I would appreciate
contributions of code or money, so that I can continue to maintain and
improve AMPiI.

If you find AMPiI useful in a professional context and would like my
help, I may be willing to work for you, as a contractor, consultant,
or employee.

# Appendix A: Developers

This manual has been intentionally left vague, as most of the features
described here are not, in fact, implemented. As features are
implemented, please update this manual to reflect the concrete useage.

# Appendix B: Installation and Configuration

As AMPiI is in very early stages, there is no installation proceedure
at this time. The intent is to offer a variety of methods:

- source code
- distro packages
- self-contained, bootable images for Raspberry PI and intel PCs

I encourage hardware developers to produce turn-key AMPiI
systems, which would integerate a linux PC, digital scale, barcode
reader and / or camera into one product, with or without the software
pre-installed -- provided the terms of the AGPL license are respected.

I'm less keen on the idea of AMPiI-powered online services, but I
admit that some users may prefer to use AMPiI this way, for obvious
reasons. For my part, this is more complexity and hassle than I can
support at this time. If you choose to go this route, you're on your
own.
