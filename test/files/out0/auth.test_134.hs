Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t3"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "x"
                    , type_ = Just "INTEGER"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'PrimaryKey Ascending Abort False
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "y"
                        , type_ = Nothing
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "z"
                        , type_ = Nothing
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )