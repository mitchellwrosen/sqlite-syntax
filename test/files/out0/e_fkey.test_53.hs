Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "parent"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "x"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "y"
                        , type_ = Nothing
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'Unique
                            ( IndexedColumn
                                { column = "y"
                                , collation = Nothing
                                , ordering = Ascending
                                } :|
                                [ IndexedColumn
                                    { column = "x"
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