Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t6"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "y"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "z"
                        , type_ = Nothing
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'PrimaryKey
                            ( IndexedColumn
                                { column = "y"
                                , collation = Nothing
                                , ordering = Ascending
                                } :|
                                [ IndexedColumn
                                    { column = "z"
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