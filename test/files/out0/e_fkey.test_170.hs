Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "p"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "b"
                        , type_ = Nothing
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'PrimaryKey
                            ( IndexedColumn
                                { column = "a"
                                , collation = Nothing
                                , ordering = Ascending
                                } :|
                                [ IndexedColumn
                                    { column = "b"
                                    , collation = Nothing
                                    , ordering = Ascending
                                    }
                                ]
                            ) Abort
                        }
                    ]
                , withoutRowid = False
                }
            )
        }
    )