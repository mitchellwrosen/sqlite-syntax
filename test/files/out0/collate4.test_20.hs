Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "collate4t1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Nothing
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'PrimaryKey Ascending Abort False
                            }
                        , Named
                            { name = Nothing
                            , value = ColumnConstraint'Collate "NOCASE"
                            }
                        ]
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )