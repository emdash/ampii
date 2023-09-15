# Overview

AMPiI has one fundamental job: to help you track every gram of food
that goes into your mouth. Internally, AMPiI tracks food *by weight*.

This isn't perfect, of course, but it's probably better than whatever
it is you've been doing. To go further than this would require
techniques and equipment far beyond the reach of ordinary people, and
will have to wait for a better day.

Broadly speaking, your time with AMPiI will be spent like this:
- Set up your Inventory, a one-time task.
- Import recipes, at any time
- Create a meal plan, from specific recipes
- Generate a Shopping List from a meal plan
- Update your Inventory, after a shopping trip.
- Cook dish(es) from your meal plan
- Log what you've eaten
- Review your progress, at the end of a planning period.

Although AMPiI is for everyine, it's especially oriented towards those
who want to prepare food in advance, a.k.a. *meal prepping*.

AMPiI tracks all the food you eat, whether it's pre-packaged, whole,
or prepared from scratch.

## What I'm Really Asking of You

AMPiI represents a bit of a social experiment, and a bit of retro
idea: the return of the *kitchen computer*.

I grew up in the '80s and '90s. For some reason, back then, the
*kitchen* (or near it) was the most popular place to put your
computer. There were a few reasons for this, I think: one is that few
houses had a dedicated *computer room*, and the living room was
already dominated by the TV. So the kitchen was often the only place
for a computer. Probably also, it was so mom could keep an eye on the
kids, to make sure they weren't up to any computer-related mischief.

But I think also, a lot of home cooks used their computers to manage
their recipes. I think it's time to revisit this idea. I'm going to
ask you to set up a dedicated kitchen computer.

To this computer, you attach your kitchen peripherals: your scale,
your barcode scanner, your camera, and your label printer. This way,
AMPiI can be the center of your culinary activities.

You're going to be frequently interacting with AMPiI while in the
kitchen, and this makes little sense if your computer is located in
another room.

Just know that AMPiI's needs are modest. It does not need to be a new
computer, or a fancy computer. It's utterly up to you which type of
computer you dedicated to AMPiI. But I do suggest that you *dedicate*
a computer to AMPiI. I also recommend that it have a keyboard, though
I'll do my best to make a keyboard and mouse unecessary.

You're going to be using AMPiI with dirty hands, in a place where
spills and splatters are likely, so keep that in mind when choosing
its location.

This computer does not need to be connected to the internet! AMPiI can
work entirely offline. If you do have internet access, it allows the
following:
- Synchronizing your inventory: this allows you to do planning and
  inventory on a separate computer, if you so choose, or to backup
  your data.
- Participating in the community food database
- Receive software updates to AMPiI, as they become available.
- Most importantly: setting your computer's clock automatically. This
  ensures that AMPiI records dates correctly. If you choose to run an
  offline machine, *make sure your clock is set correctly*.

# Nutrition #

In nutrition, *nutrient density* is the amount of some nutrient for
some quantity of food. It's a simple ratio, but it's often expressed
in units like "g/100g".

AMPiI normalizes all these quantities so that totals can be calculated
for any sized portion of a given food.

# Inventory

The inventory consists of two parts: a food database, with nutritional
information about each food, and a *pantry* which tracks individual
containers of food.

## What it's For

AMPiI queries the Inventory when generating shopping lists, so that
the list only contains the items that you *actually* need to purchase.

During planning, expirations dates alert you to use up food before it
expires.

## Setup

Your first task after installation will be to set up your
inventory. This is, admittedly, a bit painful. But well worth doing.

You're going to need a couple of hours, and a bit of space to work in.

Now, here's the hard part: take everything out of your pantry and
place it on your kitchen table. Set up your digital scale, barcode
scanner, and camera, and printer if you have these.

Using the Inventory workflow, you're going to add every item in your
inventory into your AMPiI database.

When you're done, you'll have a datbase of:
- how much, by weight, of each foodstuff, remains in your pantry
- the nutritional data for each distinct foodstuff
- the status of each container in your pantry

This is a big step. I get it. But you only have to do it once.

## Maintenance

After this, it's not so bad.

As you eat and prepare food, AMPiI will track the consumption of
inventory items.

When you shop, AMPiI will update the Inventory to include what you've
bought. It's the same workflow as the initial setup, however, the
longer you use AMPiI, the easier it gets.

# Recipes

Every dish starts with a recipe. How you get your recipes is totally
up to you. Online. Print. Your grandma's hand-written family
secrets. It doesn't matter, but in some fashion, they need to exist
within AMPiI.

AMPiI cares about your recipes:
- it calculates the nutrtional summary for each dish
- it generates shopping lists for the recipes you want to make
- it will help you measure each ingredient when cooking

You don't have to type in the whole recipe. AMPiI works with a
simplified notion of recipes, that essentially consists of the
ingredients list and the required quantity of each.

As stated above, AMPiI tracks food by weight. Obviously, many recipes
are given in *volumetric* units, especially where I live, in North
America.

Not to worry: AMPiI will help you convert to more scientific
weight-based measurements on the fly.

As with the inventory setup, in the beginning, you'll have more work
entering each recipe you want to make; over time, you'll be relying on
work you've already done.

A further enhancement is that you can group ingredients that go into
the recipe at the same time into *stages*.

## Recipe Cleaner

There is a browser plugin, [Recipe
Cleaner](https://github.com/erik/recipecleaner) that you may find
helpful when dealing with recipes found on the web.

## Substitutions

Sometimes an ingredient can be substituted with one or more
alternatives. AMPiI lets you capture this information precisely, to
make your shopping trips as painless as possible.

# Planning

Once you've got your inventory set up, and have the recipes you want
to make imported, it's time to make a meal plan.

At minimum, a meal plan is a set of recipes. Each recipe can be scaled
up or down, to reflect the amount of each dish you need to make. From
here, you can generate a shopping list, using the Inventory to filter
out ingredients you already have enough of, but making sure not to
leave out any key ingredients that you don't have.

If you like, you can go further, and create *menus* for a set of
*dates*. You assigns *portions* of a dish to *meal slot*. AMPiI can
then scale your recipes for you, so that you make enough to satisfy
your plan.

You can go further still, and establish *goals* for your plan. These
are *constraints* on specific nutrients. For example: no more than
2000 calories per day, at least 30 grams of fiber per day, and no more
than 2300mg of sodium per day. The what, how much, or how little, is
up to you.

AMPiI will warn you if your plan fails to satisfy these constraints,
allowing you to adjust manually. Also included is a constraint
solver. As you make adjustments, the recipes are sacled so that your
shopping list always represents the *minimum* quantities to be
purchased to satisfy fullfill your plan.

# Shopping

You can export your shopping list in a variety of formats. This is a
fertile area for contributors to explore.

The most basic format is a PDF, which you can print or transfer to
your phone.

Returning from your shopping trip, be sure to update your Inventory
with the items you've purchased.

# Cooking

This is where AMPiI really begins to shine. But this is alo where I
must ask you to change the way you cook somewhat.

You've done all this work up front to set up your inventory, import
your recipes, created your beautiful plan, done your shopping. But to
get the most out of AMPiI, you need to weigh your food.

Choose a set of dishes from your plan, and enter cooking mode.

## Check-Out

In the check-out phase, AMPiI will prompt you to pull containers from
your pantry. Enter the barcode, container ID, and log the weight of
each item. Only when all items checked in an weighed, do you proceed
to the next phase.

One thing I must ask you to do here: do not return your containers to
the pantry at this time. Keep them out, and set them aside, unless
absolutely necessary for food safety reasons. You'll see why later.

## Mise en Place

Now you're going to weigh out your ingredients, according to the
*calculated* amounts from your plan -- not your source recipe.

You don't have to be perfect. But try to get as close as you can.

If you took the time to group your ingredients by stage, it will pay
off now -- you can weigh all the items that go into each stage into
the same container.

Only when all items from all the recipes have been measured can you
proceed to the next phase.

## Cook

This part is up to you. Go wild. Or don't.

The main thing I ask, is that if you end up adding any additional
amount, of any ingredient (e.g. *salt to taste*), that it be recorded.

No cheating!

How are you going to know how much fat your dish contains, if you
aren't honest about how much oil you've *really* used? The same would
go for any other ingredient.

The good news here is that all you need to do is scan and weigh the
container again, and AMPiI will update the total.

If you're cooking multiple dishes at once, that you need to make sure you
log to the correct recipe.

### Log Batches

As you fish cooking each dish, log the *batch*.

This crates a new food item in your inventory, which is associated
with your plan.

You can assign a barcode at this time, to ease with tracking.

Also, to weigh the final amount. This is so AMPiI can refines the
*yield* of your recipe, and also calculates an accurate *nutrient
density*, for tracking purposes.

When all your batches are logged, you can proceed to the next phase.

## Check-in

Cooking can be an energetic and chaotic activity. Things
happen. Mistakes get made. There are many potential sources of
loss. You are free to return to earlier stages at any time, and begin
again.

But if you've made it to ths point at last, it's time to check all
your containers back into your pantry. This is why I asked you to set
them aside, and not return them directly to your pantry: it's much
faster to check them back in.

If you didn't, not to worry. AMPiI has kept track of the containers
you checked out, and so you should be able to find them again.

The point is: you must scan and weigh all the containers again, so
AMPiI can update the inventory. If a container was entirely consumed,
you can mark it as such and skip the waying step.

# Tracking

If you have made a meal plan, you'll see it displayed, with the
current or upcoming meal slot selected. You can select any meal in
your plan, and the details will be displayed.

You can log meals that were eaten *as planned* with the *log as
planned* command.

Or, you can enter the amounts manually.

## Portion Mode

If you have a scale, AMPiI can help you portion food according to your
meal plan.

Upon entering portion mode, AMPiI will prompt you to place your empty
contanier on the scale.

- Put your plate, bowl, or other empty container on the scale.
- Scan the indicated container: AMPiI tares the scale.
- Add food from the container, until the portion is as desired. You
  don't need to be perfect, but AMPiI's feedback will make it easy to
  be accurate.
- Scan the next container. AMPiI will tare the weight, and the process repeats.
- AMPiI will let you know when all the items on the menu have been portioned.
- You can, however, coninue scanning containers. For example, say you're
  adding some condiments or sides that weren't on the menu. Try to
  track *everything* you put on your plate.

When you're finished, commit the apportionment to your food log. AMPiI
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

As part of your plan, you may have speficied biometric goals to be
tracked. This goes by the shorthand "weigh-in". But it need not be
limited to, or even involve your weight.

Track whatever means most to you: your BMI, your body fat percentage,
your cholesterol, your blood pressure, even your mood.

However often you told AMPiI you wanted to track this information is
how often AMPiI will nudge you to record it.

Be honest. After all, you'd only be only cheating yourself.

# Review Mode

When you've reached the end of your meal plan, AMPiI will nag you to
review. Pick a time when you can focus on it, and enter review mode.

AMPiI will show you a summary chart. You'll see your "weigh-in" score:
i.e., how consistent you were with weigh ins.

You'll see your biometric goal chart plotted against your reported figures.

You'll see your nutritional goal chart, plotted against your daily
totals.

This is your time to reflect on your results. You should ask yourself
the following questions:

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

If you manage to build a business around it, I would appreciate a
monetary contribution so that I can continue to maintain and improve
AMPiI.

# Appendix A: Developers

This manual has been intentionally left vague, as most of the features
described here are not, in fact, implemented.

As features are implemented, please update this manual to reflect
the concrete useage.

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

I would encourage hardware developers to produce turn-key AMPiI
systems, which would integerate a linux PC, digital scale, barcode
reader and / or camera into one product, with or without the software
pre-installed -- provided the terms of the AGPL license are respected.

I'm less keen on the idea of AMPiI-powered online services, but I will
admit that some users may prefer to use AMPiI in this way, for obvious
reasons. For my part, this is more complexity and headache than I am
willing to support at this time. If you choose to go this route, for
the time being at least, you're on your own.
