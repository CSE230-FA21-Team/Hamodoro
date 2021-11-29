# Project
CSE230 FA21 Final Project: Hamodoro (A Pomodoro Timer in Haskell)

## Proposal


---

## Overview

The Pomodoro Technique is a time management method developed in the late 1980s. In this project, we are going to implement a Pomodoro timer that breaks work into intervals for improving productivity. With our program, a user can decide on tasks to be done, label them separately, add them to his/her calendar, set the timer to start working, stop working when the timer rings and take a short break, etc. A fully-functioning Pomodoro timer is a great tool for individuals & teams to visualize their daily routine and manage their working style effectively.

## Goals/Features

+ Countdown Timer (clock analog or digital)

There will be a countdown timer on the screen, which will start counting down when we set the session time and start the timer. At this point of time, we have two plans for the timer. One is to work out a clock analog with clock hands, and the other is to use a digital representation. We will choose the one that works better.

+ Self-defined Session Title

When we set a session, besides the start time and duration time, we can also input a title for that session. The title can be shown in the schedule table for the session, so that it will be much easier to check the arrangement of a day.

+ Notification

Working for a long period of time is ineffective and taking a rest is also an important part of the users’ work. At the end of each Pomodoro session, a notification will be pushed to congratulate the users on the completion of a session and to remind them to take a rest. 
	
+ Schedule Visualization

We also integrate our Pomodoro clock with the user's calendar schedule so that the users can see their day at glance to plan ahead and take better advantage of the Pomodoro clock. The calendar will be present beside the timer and the users can type their schedule of the day into this calendar. 

+ Notes during Session

During a focus session, users will be able to write down their notes and thoughts in the “Notes” section of the app. The notes will be shown in the calendar for later review, and stored locally as plain text files. We will aim for markdown support as a stretch goal.  

+ Keyboard Control

Our app will support full keyboard control. We provide keyboard shortcuts to functions like start/pause timer, switch layout, etc.  Due to the limitations of the brick library, most of our shortcuts will be a single letter key without modifiers.


## Project Members

- Shujie Chen ([@Al203](https://github.com/Al203))
- Bowei Li ([@7bw](https://github.com/7bw))
- Yi Rong ([@LER0ever](https://github.com/LER0ever))
- Ximing Wang ([@wang-ximing](https://github.com/wang-ximing))

## Links
Project requirements: https://ucsd-cse230.github.io/fa21/project.html  
Brick library: https://github.com/jtdaugherty/brick/  
Proposal form: https://forms.gle/Fd4CUajSCzS4Er4e7  

## Milestones
- [Milestone 1: Registration and Proposal (#2)](https://github.com/CSE230-FA21-Team/Project/issues/2)

## Milestone 2: Updates

### Key Components

We are presenting a module that includes all features our application supports, including starting/reseting timer, editing/deleting notes, pushing notifications, restarting the whole application, etc. The changes of states are controlled in Actions.hs.

Also, we are implementing our countdown timer as one of our key components. We place a widget on the left, inside which are large numbers showing the remaining time. Another key component is the schedule table on the right, showing all the scheduled tasks with their time spans and notes. There is also a component widget for users to edit tasks. Users can add tasks to the scheduler, and each task has its own title, content, and corresponding duration of time that will be shown on the timer.

Then we are implementing the UI based on the functionalities of our program using the brick library, displaying the necessary contents on the front end to make our application fully functional. We are using the brick library’s ticking function to update the interface every second and on user inputs, and we are also using Brick’s keyboard events to handle shortcuts and editor area text input. 

### Project structure:

∅ tree
.
├── app
│   └── Main.hs
├── LICENSE
├── Makefile
├── package.yaml
├── README.md
├── Setup.hs
├── src
│   ├── Actions.hs
│   ├── Config.hs
│   ├── Lib.hs
│   └── UI
│       ├── Editor.hs
│       ├── Sheet.hs
│       ├── Stats.hs
│       └── Style.hs
├── stack.yaml
├── stack.yaml.lock
├── test
│   └── Spec.hs
└── hamodoro.cabal

### Challenges

The graph implementation of the clock UI is time consuming. It is hard to implement a round clock face or the ticking clock hands using the Brick library. So in order to implement our functionality, we decided to switch to an easier alternative. We choose to embed all the clock numbers into our code by drawing number sign strings. The end result of the clock looks like the graph below. It is not only good-looking, but also serves well to show the remaining time.


## License
The project is open sourced under the terms of BSD 3-Clause License, details of which can be found in the [`LICENSE`](LICENSE) file
