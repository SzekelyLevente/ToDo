namespace ToDo

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open System.IO

type Todo={name:string ; isDone:bool ; date:System.DateTimeOffset}

module Operation=
    let replace list value newValue =
        list |> List.map(fun item -> if item = value then newValue else item)

    let writeFile (filePath:string) (content:string list)=
        File.WriteAllLines(filePath,content)

    let readLinesFromFile (filePath: string) : seq<string> =
        seq {
            use reader = new StreamReader(filePath)
            while not reader.EndOfStream do
                yield reader.ReadLine()
        }

    let rec createList (list:Todo list) (lines:string seq)=
        if Seq.length lines > 0 then
            let line=Seq.head lines
            let datas=line.Split(";");
            let newSeq=Seq.tail lines
            let isDone=if int(datas.[1])=0 then false else true
            let date=System.DateTimeOffset.Parse(datas.[2])
            let newList=({name=datas.[0];isDone=isDone;date=date})::list
            if Seq.length newSeq = 0 then
                newList
            else
                createList newList newSeq
        else
            []

    let rec getListText (list:Todo list) (acc:string list)=
        if List.length list > 0 then
            let item=List.head list
            let newLine = item.name + ";" + (if item.isDone then "1" else "0") + ";" + string item.date
            let newList=List.tail list
            let text=newLine::acc
            if List.length newList = 0 then
                text
            else
                getListText newList text
        else
            []

    let save list=
        writeFile "todo.txt" (List.rev(getListText list []))

    let load=
        List.rev (readLinesFromFile "todo.txt" |> createList [])

type EditWindow(todo:Todo,(list: IWritable<Todo list>),(originalList: IWritable<Todo list>)) =
    inherit Window()
    do
        base.Title <- "Edit"
        base.Width <- 300.0
        base.Height <- 200.0
        base.Content <- 
            Component(fun ctx ->
                let Task=ctx.useState todo.name
                let Date=ctx.useState todo.date
                StackPanel.create [
                    StackPanel.children [
                        TextBox.create [
                            TextBox.text Task.Current
                            TextBox.onTextChanged (fun newText ->
                                Task.Set(newText)
                            )
                        ]
                        DatePicker.create [
                            DatePicker.selectedDate Date.Current
                            DatePicker.horizontalAlignment HorizontalAlignment.Center
                            DatePicker.onSelectedDateChanged (fun newDate ->
                                Date.Set(newDate.Value)
                            )
                        ]
                        Button.create [
                            Button.content "Edit"
                            Button.horizontalAlignment HorizontalAlignment.Center
                            Button.onClick (fun args ->
                                let newTask=Task.Current
                                let isDone=todo.isDone
                                let newDate=Date.Current
                                let newTodo={name=newTask;isDone=isDone;date=newDate}
                                let newList= Operation.replace list.Current todo newTodo
                                let newOriginalList= Operation.replace originalList.Current todo newTodo
                                list.Set(newList)
                                originalList.Set(newOriginalList)
                                Operation.save originalList.Current
                            )
                        ]
                    ]
                ]
            )

module Main =

    let getListItem (index:int) (list:IWritable<Todo list>) (originalList:IWritable<Todo list>) (listItems:IWritable<Types.IView list>):Types.IView=
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [ 
                    TextBlock.text $"{list.Current.[index].name}"
                    TextBlock.dock Dock.Left
                    TextBlock.horizontalAlignment HorizontalAlignment.Left
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ]
                Button.create [
                    Button.content "Delete"
                    Button.dock Dock.Right
                    Button.horizontalAlignment HorizontalAlignment.Right
                    Button.margin (Thickness(2))
                    Button.onClick(fun args ->
                        let newList=List.filter (fun x -> x <> list.Current.[index]) list.Current
                        let newOriginalList=List.filter (fun x -> x <> originalList.Current.[index]) originalList.Current
                        let newListItems:Types.IView list=List.filter (fun x -> x <> listItems.Current.[index]) listItems.Current
                        list.Set(newList)
                        originalList.Set(newOriginalList)
                        listItems.Set(newListItems)
                        Operation.save originalList.Current
                    )
                ]
                Button.create [
                    Button.content "Edit"
                    Button.dock Dock.Right
                    Button.horizontalAlignment HorizontalAlignment.Right
                    Button.onClick(fun args ->
                        let edit=EditWindow(list.Current.[index],list,originalList)
                        edit.Show()
                    )
                ]
                CheckBox.create [
                    CheckBox.dock Dock.Left
                    CheckBox.margin (Thickness(2))
                    CheckBox.isChecked (list.Current.[index].isDone)
                    CheckBox.onClick (fun args ->
                        let newDone=not list.Current.[index].isDone
                        let task=list.Current.[index].name
                        let date=list.Current.[index].date
                        let newTodo={name=task;isDone=newDone;date=date}
                        let newList=Operation.replace list.Current list.Current.[index] newTodo
                        let newOriginalList=Operation.replace originalList.Current list.Current.[index] newTodo
                        list.Set(newList)
                        originalList.Set(newOriginalList)
                        Operation.save originalList.Current
                    )
                ]
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.text (list.Current.[index].date.DateTime.ToString())
                ]
            ]
        ]

    let addListView (tb:IWritable<string>) (date:IWritable<System.DateTimeOffset>) (list:IWritable<Todo list>) (originalList:IWritable<Todo list>) listItems=
        let newTask=string tb.Current
        let todo={name=newTask;isDone=false;date=date.Current}
        let newList=todo::list.Current
        let newOriginalList=todo::originalList.Current
        list.Set(newList)
        originalList.Set(newOriginalList)
        let newListBoxItem=getListItem 0 list originalList listItems
        listItems.Set(newListBoxItem::listItems.Current)
        tb.Set("")
        Operation.save originalList.Current

    let initialList = Operation.load

    let view () =
        Component(fun ctx ->
            let list = ctx.useState initialList
            let originalList=ctx.useState initialList
            let tb=ctx.useState ""
            let date=ctx.useState System.DateTimeOffset.Now
            let listItems=ctx.useState []
            let search=ctx.useState ""
            let getListItems : Types.IView list = list.Current |> List.mapi (fun i t -> 
                getListItem i list originalList listItems
            )
            listItems.Set(List.rev(getListItems))

            DockPanel.create [
                DockPanel.children [
                    Button.create [
                        Button.dock Dock.Bottom
                        Button.onClick (fun _ ->
                            if tb.Current <> "" then
                                addListView tb date list originalList listItems
                        )
                        Button.content "Add task"
                        Button.horizontalAlignment HorizontalAlignment.Stretch
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                    ]
                    DatePicker.create [
                        DatePicker.dock Dock.Bottom
                        DatePicker.horizontalAlignment HorizontalAlignment.Stretch
                        DatePicker.selectedDate date.Current
                        DatePicker.onSelectedDateChanged (fun newDate ->
                            date.Set(newDate.Value)
                        )
                    ]
                    TextBox.create [
                        TextBox.dock Dock.Bottom
                        TextBox.horizontalAlignment HorizontalAlignment.Stretch
                        TextBox.horizontalContentAlignment HorizontalAlignment.Center
                        TextBox.height 25
                        TextBox.text tb.Current
                        TextBox.watermark "task"
                        TextBox.onTextChanged (fun newText -> 
                            tb.Set(newText)
                        )
                        TextBox.onKeyDown(fun args ->
                            if args.Key = Input.Key.Enter then
                                if tb.Current <> "" then
                                    addListView tb date list originalList listItems
                        )
                    ]
                    TextBox.create [
                        TextBox.dock Dock.Top
                        TextBox.watermark "search"
                        TextBox.text search.Current
                        TextBox.onTextChanged (fun newText ->
                            if newText <> "" then
                                let todos=List.filter (fun x -> x.name.Contains(newText)) originalList.Current
                                list.Set(todos)
                                let newListItems : Types.IView list = list.Current |> List.mapi (fun i t -> 
                                    getListItem i list originalList listItems
                                )
                                listItems.Set(newListItems)
                            else
                                list.Set(originalList.Current)
                                listItems.Set(getListItems)
                        )
                    ]
                    ListBox.create [
                        ListBox.dock Dock.Top
                        ListBox.viewItems (
                            getListItems
                        )
                    ]
                ]
            ]
        )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "ToDo list"
        base.Content <- Main.view ()

type App() =
    inherit Application()
    
    override this.Initialize() =
        this.Styles.Add (FluentTheme())
    
    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
