Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "t1"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'InSubquery
                ( InSubqueryExpression
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "b"
                            }
                        )
                    , subquery = SelectStatement
                        { commonTableExpressions = Nothing
                        , select = CompoundSelect
                            ( SelectCore'Select
                                ( Select
                                    { distinct = False
                                    , columns = ResultColumn'Expression
                                        ( Aliased
                                            { value = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "b"
                                                    }
                                                )
                                            , alias = Nothing
                                            }
                                        ) :| []
                                    , from = Just
                                        ( Table
                                            ( QualifiedTableName
                                                { name = Aliased
                                                    { value = Namespaced
                                                        { namespace = Nothing
                                                        , value = "t1"
                                                        }
                                                    , alias = Nothing
                                                    }
                                                , indexedBy = Nothing
                                                }
                                            )
                                        )
                                    , where_ = Just
                                        ( Expression'GreaterThan
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "a"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "8" )
                                            )
                                        )
                                    , groupBy = Nothing
                                    , window = Nothing
                                    }
                                )
                            )
                        , orderBy = Nothing
                        , limit = Nothing
                        }
                    }
                )
            )
        , returning = Nothing
        }
    )