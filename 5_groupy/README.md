# About: Groupy the Group Membership Service

When creating distributed applications, one may face the difficulty of sharing a state. In this report, a system is developed where multiple applications - ”group membership services” - share a state, which is represented by a color. These applications use a atomic multicast leader-slave subsystem to communicate state changes with one another. Step by step, upgrades are made to ensure reliability and order when dealing with failing nodes and lost messages.
