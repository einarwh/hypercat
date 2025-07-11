# Hypercat

Hypercat is a hypermedia-driven concatenative programming language. It evaluates URLs.

## Run the application locally

You need .NET installed.

Type `dotnet run --project Hypercat` in a terminal.

Open `http://localhost:5099` in a browser.

## Try Hypercat in Azure

Go to https://hyperkatt.azurewebsites.net.

Mostly just click on links and see what happens.

## Advent of Code 2022, Day 1 Example

If you would like to learn Hypercat by example, follow these instructions.

- Copy the string `1000%A02000%A03000%A0%A04000%A0%A05000%A06000%A0%A07000%A08000%A09000%A0%A010000` into the textbox, select the String radio button, and click the 'push' button. You should see the string added to your stack (and your URL).
- Repeat the process with the string `%A0%A0`.
- Click the _split_ link. This should split the long string into five parts and put it in a list. The list is a single item on the stack, ranging from the _list_ keyword to the _end_ keyword.
- Now we want to split each item in the list. For that, we'll need to map over the list, and for that, we'll need a code block.Click the _proc_ link. This should push `-proc-` onto the stack, which marks the beginning of a code block. Code blocks are ended by clicking the _end_ link, but don't do that just yet. For now, note that you have access to many more links when you're inside a code block. This is because Hypercat doesn't know what will be on the stack when the block will be executed. Since it's just a data item on the stack, it can be shuffled around.
- Push the string `%A0`onto the stack using the textbox again.
- Click the _split_ link. Note that the only thing that happens is that the name _split_ is pushed onto the stack. That's because we're still defining the code block.
- Now is the time to click the _end_ link. This completes the code block. You'll see that `-proc-` turned into `proc` and the various items in the code block are indented, to show that they are now grouped in a single item on the stack.
- Click _map_. You should now see a nested list of strings.
- We'd like the strings to be integers. This requires more mapping. Click _proc_ to start defining what should happen to each inner list.
- We need a second code block to map over each inner list. Click _proc_ again!
- Luckily, the second code block is very simple. Just click _int_ (which converts string values to integers), followed by _end_.
- To finish the outer code block, click _map_, followed by another _end_. You should now have a nested proc!
- Map your proc over the list by clicking _map_. After all that work, the strings have become integers!
- Next, we'll add up the integers in each inner list. To do that, we'll use a code block to reduce the inner lists. That code block needs to be mapped over the outer list. Click _proc_ twice, followed by _add_, _end_, _reduce_, and _end_.
- Click _map_. You should now see a list of five integers. Much simpler!
- We want to pick out the highest number. To do that, we'll sort the list in descending order. Click _sort_, followed by _rev_.
- Now the road forks.
- To solve the example in part 1 of day 1 of Advent of Code 2022, click _head_ to select the top element. This will give you 24000 as advertised.
- To solve the example in part 2, click _zero_, followed by _succ_ three times. Alternatively, type the number 3 in the textbox, select the number radio button, and click push. Either strategy should give you the number 3 on top of the stack. Click _take_ to take the first three elements of the list.
- Now we just need to add these three numbers. That's easy! Just click _proc_, _add_, _end_, and _reduce_. This will give the number 45000.
