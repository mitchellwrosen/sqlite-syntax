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
                    { name = "a"
                    , type_ = Just "text"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "b"
                        , type_ = Just "int"
                        , constraints = []
                        }
                    , ColumnDefinition
                        { name = "c"
                        , type_ = Just "float"
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'PrimaryKey
                            ( IndexedColumn
                                { column = "b"
                                , collation = Nothing
                                , ordering = Ascending
                                } :| []
                            ) Abort
                        }
                    ]
                , withoutRowid = False
                }
            )
        }
    )