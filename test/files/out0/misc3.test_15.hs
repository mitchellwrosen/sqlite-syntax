Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t2"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Just "INT"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'Unique Abort
                            }
                        ]
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )