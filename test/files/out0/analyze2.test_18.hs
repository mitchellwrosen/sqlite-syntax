Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t4"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "x"
                    , type_ = Nothing
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'Collate "test_collate"
                            }
                        ]
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )