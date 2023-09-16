# Overview

AMPiI has one fundamental job: to help you track every gram of food
that goes into your mouth. Internally, AMPiI tracks food *by weight*.

This isn't perfect, of course, but it's probably better than whatever
it is you've been doing. To go further than this would require
techniques and equipment far beyond the reach of ordinary people, and
will have to wait for a better day.

Broadly speaking, your time with AMPiI will be spent like this:
- Set up your Inventory, a one-time task.
- Import recipes, at any time.
- Create a meal plan, from specific recipes: periodically.
- Generate a Shopping List from a meal plan, periodically.
- Update your Inventory, after a shopping trip: as needed.
- Cook dish(es) from your meal plan, as needed.
- Log what you've eaten, as needed.
- Review your progress, at the end of a planning period.

AMPiI is especially oriented towards those who want to prepare
food in advance, a.k.a. *meal prepping*.

AMPiI tracks all the food you eat, whether it's pre-packaged, whole,
or prepared from scratch.

# System Requirements

AMPiI represents a bit of a social experiment, and a bit of retro
idea: the return of the *kitchen computer*. I think it's time to revisit this idea. I'm going to ask you to set up a dedicated kitchen computer.

To this computer, you attach your kitchen peripherals:
- your scale,
- your barcode scanner
- your camera
- your label printer.

This way, AMPiI can be the center of your culinary activities.

You're going to be frequently interacting with AMPiI while in the
kitchen, and this makes little sense if your computer is located in
another room.

Just know that AMPiI's performance requirements are modest. The main thing
it needs is a keyboard and USB ports. A Raspberry PI 400 would be ideal.

You're going to be using AMPiI with dirty hands, in a place where
spills and splatters are likely, so keep that in mind when choosing
your system and its location. How you mitigate these hazards is also
up to you. But you need easy access to both the scale, and the scanner.

This computer does not need to be connected to the internet! AMPiI can
work entirely offline. If you do have internet access, it allows the
following:
- Most importantly: setting your computer's clock automatically. This
  ensures that AMPiI records dates correctly. If you choose to run an
  offline machine, *make sure your clock is set correctly*.
- Synchronizing your inventory: this allows you to do planning and
  inventory on a separate computer
- if you so choose, to back up your data.
- Participating in the community food database.
- Receive software updates to AMPiI, as they become available.

# Nutrition #

In nutrition, *nutrient density* is the amount of some nutrient for
some quantity of food. It's a simple ratio of mass to mass, often expressed
in units like "grams per 100 grams".

AMPiI normalizes all these quantities for foodstuffs, so that totals can be calculated for any portion of a given food.

# Inventory

The inventory consists of two parts: a food database, with nutritional
information about each food, and a *pantry* which tracks individual
containers of a given food.

## Setup

Your first task after installation will be to take inventory. This is, admittedly, a bit painful. But well worth doing up front.

You can choose to do this peicemeal if you absolutely must. But your goal should be to eventually have 100% of your inventory tracked. This step makes all the other steps much more enjoyable.

Each container is assigned a unique ID. How you do this is up to you,
but it's strongy recommended that you use a printer to create unique barcodes that you can then stick onto each container.

Or you can use a sharpie and write them by hand. Just be aware that
you'll also be entering the container ids by hand.

AMPiI also supports "reusable barcodes". These would be barcodes
printed on magnetic material, or any other way that allows re-attaching
barcodes to containers in your pantry. I'm not aware of anyone who
manufactures such things. Hint hint. The other main use-case for this is
re-usable food-storage containers. This way, there's less waste involved.

The main rule is this: a given barcode is used for a given container. This
means that, while you can use the product UPC to identify foods, you 
can't use it to track container IDs, because they're not globally unique.
AMPiI tracks individual containers of food.

## Maintenance

You use the same inventory workflow after shopping trips.
- identify the food. This could be by searching the food database,
  or scanning the product UPC.
- update nutritional facts. make sure the values reflect what's on the
  package.
- for packaged foods
  - assign a container id.
  - weigh the container
  - enter the net weight or volume
  - enter the expiration, use-by, or best-by date
- for fresh, whole, or bulk foods,
  - it's much the same as above, but you can also top-up a reusable    
    container for foods with long shelf life.
  - also, you can calculate the expiration date from an approximate "shelf life".

As you eat and prepare food, AMPiI will track the consumption for you.

# Recipes

Every dish starts with a recipe. Well, maybe not every dish. But it at least starts with a list of ingredients.

How you get your recipes is totally
up to you. Online. Print. Your grandma's hand-written family
secrets. It doesn't matter, but you need to get them into AMPiI.
This allows AMPiI to:

- Calculate the nutritional summary for each dish.
- Generate shopping lists for the recipes you want to make.
- Help you measure each ingredient when cooking.

You don't have to type in the whole recipe. AMPiI only cares about the ingredients, and amounts. You're free to be as terse or as detailed with
other aspects of your recipe. I do suggest you take the
time to add a photo, as this makes searching recipes much more fun.

AMPiI wants to know *precisely* which food from your database you're using
in your recipe. This is a level of precision that can only be achieved
from within AMPiI, for obvious reasons. So for each ingredient, you'll be
asked to identify the food (either by searching, or by scanning), and to
input a *precise* weight.

As stated above, AMPiI tracks food by weight. Obviously, many recipes
are given in *volumetric* units, especially where I live, in North
America.

Not to worry: AMPiI will help you convert to more scientific
weight-based measurements on the fly. If the density of the food is known,
AMPiI will estimate the weight for you. If it's not, you can measure by volume when you cook, and the weight will be read off the scale. Just
keep in mind that until a *precise* weight is known, nutritional totals 
will be incomplete.

As with the inventory setup, in the beginning, you'll have a lot of data to enter; over time, you'll build up a library of favorites, and things will go more smoothly.

With complex recipes, you can group related ingredients into *bowls*. This
helps with measuring ingredients during the cooking process.

## Refining Recipes from a Batch

You can use the yield and ingredient totals from a previous batch to refine a quantities a recipe. Not only will this will help achieve more consistent results, but it will help improve the accuracy of nutrition tracking.

We haven't talked about batches yet, so don't worry if this doesn't make sense.

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
*dates*. Each menu contains some set of *meals*. You complete your menu by assigning *portions* from recipes to each *meal*. AMPiI can
then scale your recipes for you, so that you make enough to satisfy your plan.

You can go further still, and establish *goals* for your plan. These
are *constraints* on specific nutrients. For example: no more than
2000 calories per day, at least 30 grams of fiber per day, and no more
than 2300mg of sodium per day. What, how much, or how little, is
entirely up to you, but defaults to established guidelines.

AMPiI will warn you if your plan fails to satisfy these constraints,
allowing you to adjust manually. Also included is a constraint
solver, which can try to figure this out for you, or -- more importantly -- warn you if this is impossible.

The final piece to add to your plan are your *biometric* goals. This 
is used during weigh-in, and is are discussed in more detail in the chapter 
on review.

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

Now you're going to weigh out your ingredients -- according to your meal plan, not your original recipe.

If you took the time to group your ingredients into *bowls*, it will
pay off now.

The bowls to be prepared are shown. Adjust the groupings now, if necessary.

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

This part is up to you. Go wild. Or don't. 
If you don't have a recipe selected, this is where in cooking mode starts.

First, you need to scan and weigh each container that you dispense from,
but *before* you dispense from it.

No cheating!

How are you going to know how much salt or fat your dish contains, if you
aren't honest about how much you've *really* added? The same would
go for any other ingredient. Many condiments and seasonings are hidden
sources of substances that you care about tracking.

If you're cooking multiple dishes at once, make sure you *log to the correct recipe.* as you dispense each ingredient. AMPiI can't help you if you make a mistake here!

In fact, I encourage you to make one dish at a time, until you get a feel
for AMPiI's weight-based approach to cooking, though you're free to prep for
multiple dishes in advance. This way, you only have to weigh your containers
one final time, when the dish is finished.

### Log Batches

As you finish cooking each dish, log the *batch*.

This crates a new food item in your inventory, which is associated
with your plan if you're using one, and the original recipe if any.

You can assign a barcode for any leftovers at this time, to ease
tracking later.

You should also, ideally, weigh the final amount. This is so AMPiI
can refine the *yield* of your recipe, which in turn allows calculating
an accurate nutrient density for each tracked nutrient for this batch.
And this, in turn, helps you with accurate portioning and tracking.

When all your batches are logged, you can proceed to the next mode.

## Check-in

Cooking can be an energetic and chaotic activity. Things
happen. Mistakes are made. There are many potential sources of
loss.

The point is, that you cannot simply rely on what you measured out
during cooking to update your inventory. You should ideally weigh each
container before you put it away.

If a container was entirely consumed, you can mark it as such and skip
the weighing step.

If you skip doing this, your inventory will reflect the last
recorded weight for a given container, but will remain marked as
"in use" until you take inventory again.

# Tracking

If you have made a meal plan, you'll see it displayed, with the
current or upcoming meal slot selected in tracking mode. 

You can select any meal in your plan, and the details will be displayed.

You can log meals that were eaten *as planned* with the *log as
planned* command.

Or, you can enter food portions manually.

## Portion Mode

If you have a scale, AMPiI can help you portion food according to your
meal plan.

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
  want to add. It might be a problem for your diet, but it's not a problem for AMPiI. Be thorough. Be honest.

When you're finished, commit the record to your food log. AMPiI
will update your inventory for you.

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

## Weigh-In

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

AMPiI is a bit old-fahsioned. It's a bit unconventional. In that
sense, AMPiI may not be to everyone's liking. That's fine with
me. It's open-source. You are free to modify the source code, per the
terms of the AGPL.

If you use it, and like it, please let me know.

If you manage to build a business around it, I would appreciate
contributions of code or money, so that I can continue to maintain and 
improve AMPiI. I may also be willing to work for you, as a contractor or consultant.

# Appendix A: Developers

This manual has been intentionally left vague, as most of the features
described here are not, in fact, implemented. As features are implemented,
please update this manual to reflect the concrete useage.

To build AMPiI, run the command:

	$ pack build ampii.idr

To run AMPiI, run the command:

	$ pack run ampii.idr

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
support at this time. If you choose to go this route, you're on your own.