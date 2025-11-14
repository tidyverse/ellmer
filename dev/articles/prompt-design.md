# Prompt design

This vignette gives you some advice about how to use ellmer to write
prompts. We’ll work through two hopefully relevant examples: a prompt
that generates code and another that extracts structured data. If you’ve
never written a prompt, I’d highly recommend reading Ethan Mollick’s
[Getting started with AI: Good enough
prompting](https://www.oneusefulthing.org/p/getting-started-with-ai-good-enough).
I think understanding his analogy about how AI works will really help
you get started:

> Treat AI like an infinitely patient new coworker who forgets
> everything you tell them each new conversation, one that comes highly
> recommended but whose actual abilities are not that clear. … Two parts
> of this are analogous to working with humans (being new on the job and
> being a coworker) and two of them are very alien (forgetting
> everything and being infinitely patient). We should start with where
> AIs are closest to humans, because that is the key to good-enough
> prompting

As well as learning general prompt design skills, it’s also a good idea
to read any specific advice for the model that you’re using. Here are
some pointers to the prompt design guides of some of the most popular
models:

- [Claude](https://docs.claude.com/en/docs/build-with-claude/prompt-engineering/overview)
- [OpenAI](https://platform.openai.com/docs/guides/prompt-engineering)
- [Gemini](https://ai.google.dev/gemini-api/docs/prompting-intro)

If you have a claude account, you can use its
[prompt-generator](https://docs.claude.com/en/docs/build-with-claude/prompt-engineering/prompt-generator).
It’s specifically tailored for Claude, but I suspect it will help you
with many other LLMs, or at least give you some ideas as to what else to
include in your prompt.

``` r
library(ellmer)
```

## Best practices

It’s highly likely that you’ll end up writing long, possibly multi-page
prompts. To ensure your success with this task, we have two
recommendations. First, put each prompt its own, separate file. Second,
write the prompts using markdown. The reason to use markdown is that
it’s quite readable to LLMs (and humans), and it allows you to do things
like use headers to divide up a prompt into sections and itemised lists
to enumerate multiple options. You can see some examples of this style
of prompt here:

- <https://github.com/posit-dev/shiny-assistant/blob/main/shinyapp/app_prompt_python.md>
- <https://github.com/jcheng5/py-sidebot/blob/main/prompt.md>
- <https://github.com/simonpcouch/chores/tree/main/inst/prompts>
- <https://github.com/cpsievert/aidea/blob/main/inst/app/prompt.md>

In terms of file names, if you only have one prompt in your project,
call it `prompt.md`. If you have multiple prompts, give them informative
names like `prompt-extract-metadata.md` or `prompt-summarize-text.md`.
If you’re writing a package, put your prompt(s) in `inst/prompts`,
otherwise it’s fine to put them in the project’s root directory.

Your prompts are going to change over time, so we’d highly recommend
commiting them to a git repo. That will ensure that you can easily see
what has changed, and that if you accidentally make a mistake you can
easily roll back to a known good verison.

If your prompt includes dynamic data, use
[`ellmer::interpolate_file()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
to intergrate it into your prompt.
[`interpolate_file()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
works like [glue](https://glue.tidyverse.org) but uses `{{ }}` instead
of [`{ }`](https://rdrr.io/r/base/Paren.html) to make it easier to work
with JSON.

As you iterate the prompt, it’s a good idea to build up a small set of
challenging examples that you can regularly re-check with your latest
version of the prompt. Currently you’ll need to do this by hand, but we
hope to eventually provide tools that’ll help you do this a little more
formally.

Unfortunately, you won’t see these best practices in action in this
vignette since we’re keeping the prompts short and inline to make it
easier for you to grok what’s going on.

## Code generation

Let’s explore prompt design for a simple code generation task:

``` r
question <- "
  How can I compute the mean and median of variables a, b, c, and so on,
  all the way up to z, grouped by age and sex.
"
```

I’ll use
[`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
for this problem because in our experience it does the best job of
generating code.

### Basic flavour

When I don’t provide a system prompt, I sometimes get answers in
different languages or different styles of R code:

``` r
chat <- chat_anthropic()
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> # Computing Mean and Median by Groups
    #> 
    #> Here are several approaches depending on your tool:
    #> 
    #> ## **R (tidyverse)**
    #> ```r
    #> library(dplyr)
    #> 
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(across(a:z, 
    #>                    list(mean = ~mean(., na.rm = TRUE),
    #>                         median = ~median(., na.rm = TRUE)),
    #>                    .names = "{.col}_{.fn}"))
    #> ```
    #> 
    #> ## **Python (pandas)**
    #> ```python
    #> import pandas as pd
    #> 
    #> df.groupby(['age', 'sex']).agg(
    #>     {col: ['mean', 'median'] for col in df.columns if col not in 
    #> ['age', 'sex']}
    #> )
    #> ```
    #> 
    #> Or more explicitly for columns a-z:
    #> ```python
    #> cols = [chr(i) for i in range(ord('a'), ord('z')+1)]
    #> df.groupby(['age', 'sex'])[cols].agg(['mean', 'median'])
    #> ```
    #> 
    #> ## **SQL**
    #> ```sql
    #> SELECT 
    #>   age, 
    #>   sex,
    #>   AVG(a) as a_mean, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY a) as 
    #> a_median,
    #>   AVG(b) as b_mean, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY b) as 
    #> b_median,
    #>   -- ... repeat for c through z
    #> FROM table
    #> GROUP BY age, sex
    #> ```
    #> 
    #> ## **Base R**
    #> ```r
    #> aggregate(. ~ age + sex, 
    #>           data = df, 
    #>           FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
    #>                               median = median(x, na.rm = TRUE)))
    #> ```
    #> 
    #> Which approach works best for you depends on your data format and 
    #> preferred language!

I can ensure that I always get R code in a specific style by providing a
system prompt:

``` r
chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers the tidyverse.
"
)
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> Here's how to compute the mean and median of variables `a` through 
    #> `z`, grouped by `age` and `sex`:
    #> 
    #> ```r
    #> library(tidyverse)
    #> 
    #> # Assuming your data frame is called 'df'
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(
    #>     across(
    #>       a:z,
    #>       list(mean = mean, median = median),
    #>       .names = "{.col}_{.fn}"
    #>     ),
    #>     .groups = "drop"
    #>   )
    #> ```
    #> 
    #> This will create columns like `a_mean`, `a_median`, `b_mean`, 
    #> `b_median`, etc.
    #> 
    #> **Alternative approach** if you want the results in long format:
    #> 
    #> ```r
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(
    #>     across(
    #>       a:z,
    #>       list(mean = mean, median = median)
    #>     ),
    #>     .groups = "drop"
    #>   ) %>%
    #>   pivot_longer(
    #>     cols = -c(age, sex),
    #>     names_to = c("variable", "statistic"),
    #>     names_sep = "_",
    #>     values_to = "value"
    #>   )
    #> ```
    #> 
    #> **Notes:**
    #> - If you have missing values, add `na.rm = TRUE`: `list(mean = 
    #> ~mean(., na.rm = TRUE), median = ~median(., na.rm = TRUE))`
    #> - The `a:z` syntax selects all columns from `a` to `z` in order
    #> - `.groups = "drop"` removes the grouping structure from the result

Note that I’m using both a system prompt (which defines the general
behaviour) and a user prompt (which asks the specific question). You
could put all this content in the user prompt and get similar results,
but I think it’s helpful to use both to cleanly divide the general
framing of the response from the specific questions you ask.

Since I’m mostly interested in the code, I ask it to drop the
explanation and sample data:

``` r
chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers the tidyverse.
  Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> ```r
    #> library(tidyverse)
    #> 
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(across(a:z, list(mean = mean, median = median), .names = 
    #> "{.col}_{.fn}"))
    #> ```

And of course, if you want a different style of R code, just ask for it:

``` r
chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers data.table.
  Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> ```r
    #> library(data.table)
    #> 
    #> dt[, lapply(.SD, function(x) list(mean = mean(x, na.rm = TRUE), 
    #>                                    median = median(x, na.rm = TRUE))),
    #> 
    #>    by = .(age, sex), 
    #>    .SDcols = letters]
    #> ```
    #> 
    #> If you want mean and median in separate columns:
    #> 
    #> ```r
    #> library(data.table)
    #> 
    #> dt[, c(lapply(.SD, mean, na.rm = TRUE), 
    #>        lapply(.SD, median, na.rm = TRUE)), 
    #>    by = .(age, sex), 
    #>    .SDcols = letters]
    #> ```
    #> 
    #> Or with prefixes:
    #> 
    #> ```r
    #> library(data.table)
    #> 
    #> result <- dt[, c(
    #>   setNames(lapply(.SD, mean, na.rm = TRUE), paste0("mean_", letters)),
    #>   setNames(lapply(.SD, median, na.rm = TRUE), paste0("median_", 
    #> letters))
    #> ), by = .(age, sex), .SDcols = letters]
    #> ```

``` r

chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers base R.
  Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> ```R
    #> aggregate(. ~ age + sex, data = your_data, FUN = function(x) c(mean = 
    #> mean(x), median = median(x)))
    #> ```
    #> 
    #> Or if you want separate columns for means and medians:
    #> 
    #> ```R
    #> means <- aggregate(. ~ age + sex, data = your_data, FUN = mean)
    #> medians <- aggregate(. ~ age + sex, data = your_data, FUN = median)
    #> colnames(means)[-(1:2)] <- paste0(colnames(means)[-(1:2)], "_mean")
    #> colnames(medians)[-(1:2)] <- paste0(colnames(medians)[-(1:2)], 
    #> "_median")
    #> merge(means, medians, by = c("age", "sex"))
    #> ```

### Be explicit

If there’s something about the output that you don’t like, try being
more explicit. For example, the code isn’t styled quite how I’d like it,
so I provide more details about what I do want:

``` r
chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers the tidyverse.
  Just give me the code. I don't want any explanation or sample data.

  Follow the tidyverse style guide:
  * Spread long function calls across multiple lines.
  * Where needed, always indent function calls with two spaces.
  * Only name arguments that are less commonly used.
  * Always use double quotes for strings.
  * Use the base pipe, `|>`, not the magrittr pipe `%>%`.
"
)
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> 
    #> ```R
    #> library(dplyr)
    #> 
    #> data |>
    #>   group_by(age, sex) |>
    #>   summarise(across(
    #>     a:z,
    #>     list(mean = mean, median = median),
    #>     .names = "{.col}_{.fn}"
    #>   ))
    #> ```

This still doesn’t yield exactly the code that I’d write, but it’s
pretty close.

You could provide a different prompt if you were looking for more
explanation of the code:

``` r
chat <- chat_anthropic(
  system_prompt = "
  You are an expert R teacher.
  I am a new R user who wants to improve my programming skills.
  Help me understand the code you produce by explaining each function call with
  a brief comment. For more complicated calls, add documentation to each
  argument. Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> ```r
    #> # Load the dplyr package for data manipulation
    #> library(dplyr)
    #> 
    #> # Compute mean and median for variables a through z, grouped by age 
    #> and sex
    #> result <- your_data %>%
    #>   # Group the data by age and sex
    #>   group_by(age, sex) %>%
    #>   # Calculate mean and median for each variable from a to z
    #>   summarise(
    #>     # across() applies functions to multiple columns
    #>     across(
    #>       # Select columns a through z
    #>       a:z,
    #>       # List of functions to apply: mean and median
    #>       list(
    #>         mean = ~mean(.x, na.rm = TRUE),  # Calculate mean, removing NA
    #> values
    #>         median = ~median(.x, na.rm = TRUE)  # Calculate median, 
    #> removing NA values
    #>       ),
    #>       # Name the output columns as "variable_function" (e.g., 
    #> "a_mean", "a_median")
    #>       .names = "{.col}_{.fn}"
    #>     ),
    #>     # Remove grouping message
    #>     .groups = "drop"
    #>   )
    #> ```

### Teach it about new features

You can imagine LLMs as being a sort of an average of the internet at a
given point in time. That means they will provide popular answers, which
will tend to reflect older coding styles (either because the new
features aren’t in their index, or the older features are so much more
popular). So if you want your code to use specific newer language
features, you might need to provide the examples yourself:

``` r
chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer.
  Just give me the code; no explanation in text.
  Use the `.by` argument rather than `group_by()`.
  dplyr 1.1.0 introduced per-operation grouping with the `.by` argument.
  e.g., instead of:

  transactions |>
    group_by(company, year) |>
    mutate(total = sum(revenue))

  write this:
  transactions |>
    mutate(
      total = sum(revenue),
      .by = c(company, year)
    )
"
)
#> Using model = "claude-sonnet-4-5-20250929".
chat$chat(question)
```

    #> 
    #> ```r
    #> data |>
    #>   summarize(
    #>     across(a:z, list(mean = mean, median = median), .names = 
    #> "{.col}_{.fn}"),
    #>     .by = c(age, sex)
    #>   )
    #> ```

## Structured data

Providing a rich set of examples is a great way to encourage the output
to produce exactly what you want. This is known as **multi-shot
prompting**. Below we’ll work through a prompt that I designed to
extract structured data from recipes, but the same ideas apply in many
other situations.

### Getting started

My overall goal is to turn a list of ingredients, like the following,
into a nicely structured JSON that I can then analyse in R (e.g. compute
the total weight, scale the recipe up or down, or convert the units from
volumes to weights).

``` r
ingredients <- "
  ¾ cup (150g) dark brown sugar
  2 large eggs
  ¾ cup (165g) sour cream
  ½ cup (113g) unsalted butter, melted
  1 teaspoon vanilla extract
  ¾ teaspoon kosher salt
  ⅓ cup (80ml) neutral oil
  1½ cups (190g) all-purpose flour
  150g plus 1½ teaspoons sugar
"
```

(This isn’t the ingredient list for a real recipe but it includes a
sampling of styles that I encountered in my project.)

If you don’t have strong feelings about what the data structure should
look like, you can start with a very loose prompt and see what you get
back. I find this a useful pattern for underspecified problems where the
heavy lifting lies with precisely defining the problem you want to
solve. Seeing the LLM’s attempt to create a data structure gives me
something to react to, rather than having to start from a blank page.

``` r
instruct_json <- "
  You're an expert baker who also loves JSON. I am going to give you a list of
  ingredients and your job is to return nicely structured JSON. Just return the
  JSON and no other commentary.
"

chat <- chat_openai(instruct_json)
#> Using model = "gpt-4.1".
chat$chat(ingredients)
```

    #> ```json
    #> [
    #>   {
    #>     "name": "dark brown sugar",
    #>     "quantity": 0.75,
    #>     "unit": "cup",
    #>     "metric_quantity": 150,
    #>     "metric_unit": "g"
    #>   },
    #>   {
    #>     "name": "eggs",
    #>     "quantity": 2,
    #>     "unit": "large"
    #>   },
    #>   {
    #>     "name": "sour cream",
    #>     "quantity": 0.75,
    #>     "unit": "cup",
    #>     "metric_quantity": 165,
    #>     "metric_unit": "g"
    #>   },
    #>   {
    #>     "name": "unsalted butter",
    #>     "quantity": 0.5,
    #>     "unit": "cup",
    #>     "metric_quantity": 113,
    #>     "metric_unit": "g",
    #>     "note": "melted"
    #>   },
    #>   {
    #>     "name": "vanilla extract",
    #>     "quantity": 1,
    #>     "unit": "teaspoon"
    #>   },
    #>   {
    #>     "name": "kosher salt",
    #>     "quantity": 0.75,
    #>     "unit": "teaspoon"
    #>   },
    #>   {
    #>     "name": "neutral oil",
    #>     "quantity": 0.333,
    #>     "unit": "cup",
    #>     "metric_quantity": 80,
    #>     "metric_unit": "ml"
    #>   },
    #>   {
    #>     "name": "all-purpose flour",
    #>     "quantity": 1.5,
    #>     "unit": "cup",
    #>     "metric_quantity": 190,
    #>     "metric_unit": "g"
    #>   },
    #>   {
    #>     "name": "sugar",
    #>     "quantity": 150,
    #>     "unit": "g"
    #>   },
    #>   {
    #>     "name": "sugar",
    #>     "quantity": 1.5,
    #>     "unit": "teaspoon"
    #>   }
    #> ]
    #> ```

(I don’t know if the additional colour, “You’re an expert baker who also
loves JSON”, does anything, but I like to think this helps the LLM get
into the right mindset of a very nerdy baker.)

### Provide examples

This isn’t a bad start, but I prefer to cook with weight and I only want
to see volumes if weight isn’t available so I provide a couple of
examples of what I’m looking for. I was pleasantly suprised that I can
provide the input and output examples in such a loose format.

``` r
instruct_weight <- r"(
  Here are some examples of the sort of output I'm looking for:

  ¾ cup (150g) dark brown sugar
  {"name": "dark brown sugar", "quantity": 150, "unit": "g"}

  ⅓ cup (80ml) neutral oil
  {"name": "neutral oil", "quantity": 80, "unit": "ml"}

  2 t ground cinnamon
  {"name": "ground cinnamon", "quantity": 2, "unit": "teaspoon"}
)"

chat <- chat_openai(paste(instruct_json, instruct_weight))
#> Using model = "gpt-4.1".
chat$chat(ingredients)
#> [
#>   {"name": "dark brown sugar", "quantity": 150, "unit": "g"},
#>   {"name": "large eggs", "quantity": 2, "unit": "count"},
#>   {"name": "sour cream", "quantity": 165, "unit": "g"},
#>   {"name": "unsalted butter", "quantity": 113, "unit": "g"},
#>   {"name": "vanilla extract", "quantity": 1, "unit": "teaspoon"},
#>   {"name": "kosher salt", "quantity": 0.75, "unit": "teaspoon"},
#>   {"name": "neutral oil", "quantity": 80, "unit": "ml"},
#>   {"name": "all-purpose flour", "quantity": 190, "unit": "g"},
#>   {"name": "sugar", "quantity": 150, "unit": "g"},
#>   {"name": "sugar", "quantity": 1.5, "unit": "teaspoon"}
#> ]
```

Just providing the examples seems to work remarkably well. But I found
it useful to also include a description of what the examples are trying
to accomplish. I’m not sure if this helps the LLM or not, but it
certainly makes it easier for me to understand the organisation of the
whole prompt and check that I’ve covered the key pieces I’m interested
in.

``` r
instruct_weight <- r"(
  * If an ingredient has both weight and volume, extract only the weight:

  ¾ cup (150g) dark brown sugar
  [
    {"name": "dark brown sugar", "quantity": 150, "unit": "g"}
  ]

* If an ingredient only lists a volume, extract that.

  2 t ground cinnamon
  ⅓ cup (80ml) neutral oil
  [
    {"name": "ground cinnamon", "quantity": 2, "unit": "teaspoon"},
    {"name": "neutral oil", "quantity": 80, "unit": "ml"}
  ]
)"
```

This structure also allows me to give the LLMs a hint about how I want
multiple ingredients to be stored, i.e. as an JSON array.

I then iterated on the prompt, looking at the results from different
recipes to get a sense of what the LLM was getting wrong. Much of this
felt like I waws iterating on my own understanding of the problem as I
didn’t start by knowing exactly how I wanted the data. For example, when
I started out I didn’t really think about all the various ways that
ingredients are specified. For later analysis, I always want quantities
to be number, even if they were originally fractions, or the if the
units aren’t precise (like a pinch). It made me realise that some
ingredients are unitless.

``` r
instruct_unit <- r"(
* If the unit uses a fraction, convert it to a decimal.

  ⅓ cup sugar
  ½ teaspoon salt
  [
    {"name": "dark brown sugar", "quantity": 0.33, "unit": "cup"},
    {"name": "salt", "quantity": 0.5, "unit": "teaspoon"}
  ]

* Quantities are always numbers

  pinch of kosher salt
  [
    {"name": "kosher salt", "quantity": 1, "unit": "pinch"}
  ]

* Some ingredients don't have a unit.
  2 eggs
  1 lime
  1 apple
  [
    {"name": "egg", "quantity": 2},
    {"name": "lime", "quantity": 1},
    {"name", "apple", "quantity": 1}
  ]
)"
```

You might want to take a look at the [full
prompt](https://gist.github.com/hadley/7688b4dd1e5e97b800c6d7d79e437b48)
to see what I ended up with.

### Structured data

Now that I’ve iterated to get a data structure I like, it seems useful
to formalise it and tell the LLM exactly what I’m looking for when
dealing with structured data. This guarantees that the LLM will only
return JSON, that the JSON will have the fields that you expect, and
that ellmer will convert it into an R data structure.

``` r
type_ingredient <- type_object(
  name = type_string("Ingredient name"),
  quantity = type_number(),
  unit = type_string("Unit of measurement")
)

type_ingredients <- type_array(type_ingredient)

chat <- chat_openai(c(instruct_json, instruct_weight))
#> Using model = "gpt-4.1".
chat$chat_structured(ingredients, type = type_ingredients)
#> # A tibble: 10 × 3
#>    name              quantity unit    
#>    <chr>                <dbl> <chr>   
#>  1 dark brown sugar    150    g       
#>  2 large eggs            2    piece   
#>  3 sour cream          165    g       
#>  4 unsalted butter     113    g       
#>  5 vanilla extract       1    teaspoon
#>  6 kosher salt           0.75 teaspoon
#>  7 neutral oil          80    ml      
#>  8 all-purpose flour   190    g       
#>  9 sugar               150    g       
#> 10 sugar                 1.5  teaspoon
```

### Capturing raw input

One thing that I’d do next time would also be to include the raw
ingredient names in the output. This doesn’t make much difference in
this simple example but it makes it much easier to align the input with
the output and to start developing automated measures of how well my
prompt is doing.

``` r
instruct_weight_input <- r"(
  * If an ingredient has both weight and volume, extract only the weight:

    ¾ cup (150g) dark brown sugar
    [
      {"name": "dark brown sugar", "quantity": 150, "unit": "g", "input": "¾ cup (150g) dark brown sugar"}
    ]

  * If an ingredient only lists a volume, extract that.

    2 t ground cinnamon
    ⅓ cup (80ml) neutral oil
    [
      {"name": "ground cinnamon", "quantity": 2, "unit": "teaspoon", "input": "2 t ground cinnamon"},
      {"name": "neutral oil", "quantity": 80, "unit": "ml", "input": "⅓ cup (80ml) neutral oil"}
    ]
)"
```

I think this is particularly important if you’re working with even less
structured text. For example, imagine you had this text:

``` r
recipe <- r"(
  In a large bowl, cream together one cup of softened unsalted butter and a
  quarter cup of white sugar until smooth. Beat in an egg and 1 teaspoon of
  vanilla extract. Gradually stir in 2 cups of all-purpose flour until the
  dough forms. Finally, fold in 1 cup of semisweet chocolate chips. Drop
  spoonfuls of dough onto an ungreased baking sheet and bake at 350°F (175°C)
  for 10-12 minutes, or until the edges are lightly browned. Let the cookies
  cool on the baking sheet for a few minutes before transferring to a wire
  rack to cool completely. Enjoy!
)"
```

Including the input text in the output makes it easier to see if it’s
doing a good job:

``` r
chat <- chat_openai(c(instruct_json, instruct_weight_input))
#> Using model = "gpt-4.1".
chat$chat(recipe)
#> [
#>   {"name": "unsalted butter", "quantity": 1, "unit": "cup", "input": 
#> "one cup of softened unsalted butter"},
#>   {"name": "white sugar", "quantity": 0.25, "unit": "cup", "input": "a
#> quarter cup of white sugar"},
#>   {"name": "egg", "quantity": 1, "unit": "unit", "input": "an egg"},
#>   {"name": "vanilla extract", "quantity": 1, "unit": "teaspoon", 
#> "input": "1 teaspoon of vanilla extract"},
#>   {"name": "all-purpose flour", "quantity": 2, "unit": "cup", "input":
#> "2 cups of all-purpose flour"},
#>   {"name": "semisweet chocolate chips", "quantity": 1, "unit": "cup", 
#> "input": "1 cup of semisweet chocolate chips"}
#> ]
```

When I ran it while writing this vignette, it seemed to be working out
the weight of the ingredients specified in volume, even though the
prompt specifically asks it not to. This may suggest I need to broaden
my examples.

## Token usage

| provider  | model                      | input | output | cached_input |  price |
|:----------|:---------------------------|------:|-------:|-------------:|-------:|
| Anthropic | claude-sonnet-4-5-20250929 |   802 |   1692 |            0 | \$0.03 |
| OpenAI    | gpt-4.1                    |  1124 |   1023 |            0 | \$0.01 |
