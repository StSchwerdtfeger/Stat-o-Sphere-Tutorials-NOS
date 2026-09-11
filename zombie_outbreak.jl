
#######################################################
#     Spatial Zombie Outbreak Simulation in Julia     #
#  Conversion of R Code from R_Basics I Chapter 9.11  #
#######################################################

# Conversion of R code into Julia code, tested in Version 1.12.1.
# Function results in a different outcome every time it is executed (incl. randomness / is stochastic).

# Code below inspired by the following math youtube video:
# https://www.youtube.com/watch?v=g-g-NdyYwsY
# For a more academic approach on Zombies using a modification of the SIR model,
# see this paper: https://doi.org/10.48550/arXiv.1503.01104

# Importing packages via "using" in Julia:
using Random
using Plots

# Percolation point/threshold for a 2D lattice (given left/right/up/down neighbors
# can be infected and no diagonal infection is possible!) 
# See Wikipedia for more details: https://en.wikipedia.org/wiki/Percolation_threshold  
percentage = 0.592746050792

# Zombie infection function, which is used within the zombie_outbreak function.
# NOTE on Julia/R differences: R has a so-called copy-on-modify semantic, meaning that 
# modifying the argument t2 of the function infect() wont change the values
# of that variable outside the function! So inside the Julia version of
# of infect(), the "t2" argument is a private copy and the caller's "t2"
# is untouched until it is reassigned. Julia arrays are passed by reference, so
# mutating "t2" in place here would also (for R users "oddly") change the input matrix 
# breaking the temporal variable swapping of t2 becoming the "new" t1 when the while loop
# proceeeds in "time". Time her refers to the steps of the while loop, defined as represeting 1 day  
# in zombie_outbreak(). However, the syntactic difference is why we take an 
# explicit copy via copy() of the inout variable in Julia. 
function infect(infected, t2)
    # Copy of t2, since it is not done right away, as in R. 
    result = copy(t2)

    # If statement to rule out positions at the matrix corner.
    # NOTE that corners and borders could be handled via more if statements, however, it is 
    # much more efficient by just adding a redundant frame with all zeros around 
    # the matrix t2 and delete it again at the end, saving the need of any further exceptions 
    # via loads of if statements for the starting and end cases....
    # The redundant frame is added within the function zombie_outbreak(percentage) in R, 
    # where percentage refers to the percentage of people over empty spaces, such 
    # that .6 means 60 % people and 40 % empty spaces... The console output refers to 
    # "amount of people" in a rather loose/analogue sense... In Julia however, all of the above
    # so-called "padding" is not done within zombie_outbreak(). In Julia an extra function
    # zero_frame() is created further below...

    # ALSO NOTE: The below function and start of the for loop in R looks like this:
    # infect = function(infected, t2){
    # for(index in 1:nrow(infected)){
    #     row = infected[index,1]
    #     col = infected[index,2]
    # The difference in Julia below is how it interprets the variable/object "infected". 
    # In R infected is the output of which(diff == FALSE, arr.ind = TRUE), i.e. alll
    # new infected within a matrix, where infected[i,1] entails the row and infected[i,2] the columns
    # position within the matrix of empty spaces, humans and infected... 
    # Julia however already understands "infected" as an 1D array list, sort of creating 
    # a kind of new object called idx, where idx[1] infected[idx,1] and idx[2] infected[idx,2] 
    # within the loop. The loop can also be written in a more classic way.... We could also
    # use "for (row, col) in infected" and directly call the variables row and col, automatically
    # related to the object infected in each loop, such that the index is handled completely in the 
    # backdroungd, without the need of calling it by name... 
    # However, it would also be possible to do it the way it is done in R, but to show some 
    # differences between R and Julia, it is done as follows:
    for idx in infected 
        row = idx[1]
        col = idx[2]

        # if (non-corner and non-border positions); size(x,1) == nrow(), size(x,2) == ncol()
        if row > 1 && row < size(result, 1) && col > 1 && col < size(result, 2)
            # Infection to the left, given that a human is at that position, i.e. 1 not 0!
            if result[row, col - 1] == 1
                result[row, col - 1] = 2
            end
            # Infection to the right
            if result[row, col + 1] == 1
                result[row, col + 1] = 2
            end
            # Infection "upwards"
            if result[row - 1, col] == 1
                result[row - 1, col] = 2
            end
            # Infection "downwards"
            if result[row + 1, col] == 1
                result[row + 1, col] = 2
            end # End if 
        end # End if
    end # End for idx in infected...

    return result
end


# zero_frame() adds a redundant frame of zeros around a matrix, so that infect() never has
# to include special-case if/else statements for corners/borders - as discussed before. 
# Mirrors the cbind()/rbind() padding (what this is called) done in the R version. It is a 
# little easier to do it this way in Julia. This process is also called padding:
function zero_frame(matrix)
    # Putting this part within the zombie_outbreak() function is possible,
    # and was done in R (see code below at the end of this comment)...
    # However, in Julia and R the padding has to be done for each t1 and t2... 
    # In Julia this is possible via creating the below line and another row2, column2 = size(t2),
    # which would be more similar to R but requires the mentioned redundant naming of row/col variabales
    # when using the size function...
    # Different to R, the following is done: we did not add rows and cols, but created a frame matrix 
    # with all 0s and added the original t1/t2 matrix on top of that empty pre-framed matrix...
    # The code in R however is much more simple, a much more readable code, handling
    # possible syntactic redundancies better imo:
    # t1 = cbind(c(0),t1,c(0))
    # t1 = rbind(c(0),t1,c(0))
    # t2 = cbind(c(0),t2,c(0))  
    # t2 = rbind(c(0),t2,c(0))
    row, column = size(matrix)
    framed = zeros(Int, row + 2, column + 2)
    framed[2:end-1, 2:end-1] .= matrix
    return framed
end 

# Actual outbreak function that uses the infect function within it:
function zombie_outbreak(percentage)

    # Define n as 10000 (more is rather hard to plot):
    n = 10000

    # Create matrix of people, empty spaces and an initial zombie0:
    # First define amount of 1s and 0s, rounded. (Named n_people/n_empty rather
    # than R's `one`/`zero`, since `one` and `zero` are built-in Julia functions.)
    n_people = Int(round(n * percentage))
    n_empty = n - n_people

    # Build a vector via repetition of 0s and 1s, using fill() (Julia's rep()-alike).
    # vcat() for certical concatenation is here used for c(); here it results
    # not in a general but a column vector...
    x = vcat(fill(1, n_people), fill(0, n_empty)) # () for c() could be enough??

    # Shuffle it — equivalent to R's sample(x) with no replacement:
    x = shuffle(x)

    # Start matrix (without infections). reshape() fills column-major just like
    # R's matrix(), so this is a direct equivalent of matrix(x, ncol = sqrt(n)):
    side = Int(sqrt(n))
    start = reshape(x, side, side) # matrix()

    # Deciding for Patient 0:
    # Search for people in the matrix (returns a Vector of CartesianIndex):
    pospeople = findall(==(1), start) # analogue to which()

    # Sample one of the people to choose a Zombie 0:
    pos0 = rand(1:length(pospeople))

    # Position of the first Zombie in the matrix.
    # Note that the expression "patient zero" comes from a reading error, confusing
    # the vowel "o" with zero... The better option would be "index case", which is
    # however not as associative to the context of an infection as when using
    # something with "zero" and naming the first person that was infected from
    # human to human patient "one"... hard to challenge conventions in this case...
    zombie0 = pospeople[pos0]

    # Add Zombie 0, initializing infection on patient 0:
    infection0 = copy(start)
    infection0[zombie0] = 2 # 2 == Zombie!!! Ahhh..!!

    # Initialize the first t1 and t2 variables (for the initial difference calculated
    # further below, which will then be repeatedly updated each loop cycle within the
    # while() loop below):
    t1 = start      # uninfected matrix
    t2 = infection0 # first infection

    # Adds a redundant frame of zeros around a matrix, so that infect() never has to
    # special-case corners/borders with extra if-statements...
    # In other words, add a redundant frame, in order to avoid writing multiple 
    # if statements in the infect() function for the case of borders and corners.
    t1 = zero_frame(t1)
    t2 = zero_frame(t2)

    # First initial difference:
    diff = t1 .== t2

    # Using a while loop for the actual infection process, which stops when the
    # current and previous state are identical, indicating that no further
    # infections are happening... We can't use a for loop here, since the number
    # of infections is dynamic and it is unclear when the infection stops in
    # advance! Therefore we use a while loop with a break included for the case
    # that the infection stopped. Otherwise the loop would go on forever
    # potentially! Makes a program crash... floods memory... leads to errors...
    steps = 1 # initial variable where +1 is added each time, to keep track of the number of cycles...

    # This vector keeps track of how many infections have occurred per step/day.
    # (NOTE: since `steps` is incremented before the first push!, the entries here
    # are shifted by one index relative to R's curve_infect[[steps]] list — the
    # sequence of values is identical, only the "day" key used to store them differs.)
    curve_infect = Int[]

    # The while loop simulates the infection over a previously unknown number of days.
    while sum(diff) < length(t1) # sum of all 0s = 0, sum of all 1s and 2s is the respective sum;
                                 # this is enough info to validate a change between t1 and t2 occurred!

        # Update variable "steps" to keep track of how many loops/cycles have passed
        # (for the final console output, stating when the outbreak found an end):
        steps += 1 ### it is a simplified way of writing steps = steps + 1, as in R!

        # Boolean matrix where false indicates a change from the previous state of
        # the matrix, i.e. indicating a new infection that leads to further infections...:
        diff = t1 .== t2

        # Stop while loop when the infection stops spreading:
        if sum(diff) == length(t1) # indicating no change between t1 and t2, then...

            # ASCII-art text to console output, including a message from the government.
            # Julia's raw"""...""" string is the equivalent of R's r"()" raw string —
            # it keeps backslashes literal instead of treating them as escape codes.
            println(raw"""
                      _     _     _             ______               _     _           _ _ _
                /\   | |   | |   | |           |___  /              | |   (_)         | | | |
               /  \  | |__ | |__ | |__            / / ___  _ __ ___ | |__  _  ___  ___| | | |
              / /\ \ | '_ \| '_ \| '_ \          / / / _ \| '_ ` _ \| '_ \| |/ _ \/ __| | | |
             / ____ \| | | | | | | | | |_ _ _   / /_| (_) | | | | | | |_) | |  __/\__ \_|_|_|
            /_/    \_\_| |_|_| |_|_| |_(_|_|_) /_____\___/|_| |_| |_|_.__/|_|\___||___(_|_|_)
            """)
            println("\t\t\t\t\t$steps Days Later\n")
            println("\t\t\t\t   BREAKING GOVERNMENT NEWS!\n")
            println("The spread of the Umbrella Corp. induced T-Virus infection has finally stopped after $steps days.")
            println("Amount of people in the initial, non-infected neighborhood of Racoon City (matrix field): $(100 / length(x) * sum(x))%")
            break
        end

        # List of the positions of previously infected aka current Zombies (current
        # loop cycle), which will be infected a few lines below via the infect()
        # function that applies the infection rule to the current matrix:
        infected = findall(!, diff) # ! represents computing FALSE instances

        # Save number of current infections given steps/days, in order to
        # optionally show the dynamic of the spread:
        ##### Push is here needed since the number of steps can not be predefined;
        ##### push here sort of appends values to a list, but is however not optimal, since it 
        ##### is not a very fast solution - however necessary in this case...
        push!(curve_infect, length(infected)) # number of new infections per step/day

        # AHHHhhh...!!! Infection of new people via the current Zombies: #######################
        new_infections = infect(infected, t2) ############ HERE is where the function infect() is used!!!!!

        # At the end of each loop, the old t2 has to become the new t1, in order to
        # keep the loop logic consistent:
        t1 = t2

        # And the new t2 has all the new infections upon the now old t2 table:
        t2 = new_infections

    end # end of while

    # Delete redundant padding frame with all zeros:
    t2 = t2[2:end-1, 2:end-1]

    # For the case that the infection has reached at least one border or not:
    # In Julia "end" as index fullfils the same function as ncol(x) or nrow(x) in R:
    if any(t2[1, :] .== 2) || any(t2[end, :] .== 2) || any(t2[:, 1] .== 2) || any(t2[:, end] .== 2)
        println("\nAhhh...!!! The infection has reached at least one border!!! Nearby districts are at risk of infection!")
        reach = 1
    else
        println("\nThe infection remained local. Sigh...")
        reach = 0
    end

    # Ratio Human/Zombie after outbreak has stopped:
    zombies = count(==(2), t2) # in R I used length(which(t2 == 2)):
    humans = count(==(1), t2)
    ratio = round(zombies / (humans + zombies), digits = 4) * 100 # convert to percentage
    # Alterantive to cat() function in R; the $ sign can call variables from the 
    # workspace and indirectly works as a delimitter, since the code is read differently,
    # not just as one ongoing character string, as the color coding implies; 
    # R uses "" for caracter strings and "," as delimitter, similar to
    # using paste() in R....
    println("\nZombies = $zombies, Humans = $humans, Ratio in Percent: $ratio % Undead Walker\n")

    # Track number of zombies and humans at the end of an outbreak:
    # Note [] instead of () results in a column vector instead if row vector,
    # just as vcat() in Julia for contacinating values to a column vector...
    # Recall, in R c() and other functions, such as array outputs are neither row or
    # column vector, just a "vector"....
    zombie_hum = [zombies, humans] 

    # Percentage of Zombies among humans:
    zombie_perc = ratio

    # Output "list" for further evaluation, especially for multiple trials.
    # Julia has no direct equivalent of R's invisible() function; using return
    # in Julia results in the same effect, does not automatically get printed
    # in the console...
    output_list = ( # analogue to list() in R...
    steps        = (steps, n),    # Number of days/steps and n, i.e. number of matrix "pixels" in total
    percentage   = percentage,    # used percentage as result of the ratio of people to total spaces/"pixels"!
    reach        = reach,         # Indicates if at least one border has been reached
    zombie_hum   = zombie_hum,    # Plain number of zombies and humans at the end of the infection
    zombie_perv  = zombie_perc,   # Percentage of zombies among people
    curve_infect = curve_infect)  # A vector of the number of new infections per step/day
    # Output elements can be called via zombie_outbreak(0.5927).steps etc.

    # Plot final state of the outbreak. Plots.jl packages heatmap() is the closest analogue
    # to R's image(); note that conventions differ between R and Julia
    # (R's image() puts row 1 at the x-axis origin which creates a transposed output
    # as it can be seen in our tutorial on FFT etc.). The picture may look rotated
    # or mirrored relative to the R version — which is hard to test, since assignement of 
    # patient 0 is random... In cgrad[:color,:color2,:color3],3,categorical = true) the
    # number 3 represents the discrete range of gradient values. Since categorical = true it
    # is referring to a discrete gradient of values...
    display(heatmap(t2,
        color = cgrad([:white, :black, :deeppink], 3, categorical = true),
        aspect_ratio = :equal, legend = false, axis = false, ticks = false,
        title = "Ahhh... Zombies!!"))

    return output_list
end # end of function zombie_outbreak

# Test function (explore what happens when using different percentages!!):
# NOTE that there are a lot of parameters returned from the above function
# that can be used to visualize more aspects of the developement of the infection,
# especially when simulating an outbreak several times. Different to the R
# version, the above is not written in a way that certain parameters can be set within ()
# of zombie_outbreak(), apart from percentage...
zombie_outbreak(0.5927)
zombie_outbreak(0.5927)
zombie_outbreak(0.5927)
