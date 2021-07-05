Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Case
                                ( CaseExpression
                                    { base = Nothing
                                    , cases =
                                        ( Expression'NotEquals
                                            ( Expression'BitwiseNegate
                                                ( Expression'LiteralValue
                                                    ( Number "11" )
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "f"
                                                    }
                                                )
                                            )
                                        , Expression'Minus
                                            ( Expression'Case
                                                ( CaseExpression
                                                    { base = Just
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "e"
                                                                }
                                                            )
                                                        )
                                                    , cases =
                                                        ( Expression'LiteralValue
                                                            ( Number "11" )
                                                        , Expression'FunctionCall
                                                            ( FunctionCallExpression
                                                                { call = FunctionCall
                                                                    { name = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "coalesce"
                                                                        }
                                                                    , arguments = FunctionArguments'Arguments
                                                                        [ Expression'Subquery
                                                                            ( SelectStatement
                                                                                { commonTableExpressions = Nothing
                                                                                , select = CompoundSelect
                                                                                    ( SelectCore'Select
                                                                                        ( Select
                                                                                            { distinct = False
                                                                                            , columns = ResultColumn'Expression
                                                                                                ( Aliased
                                                                                                    { value = Expression'Case
                                                                                                        ( CaseExpression
                                                                                                            { base = Just
                                                                                                                ( Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "d"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            , cases =
                                                                                                                ( Expression'Minus
                                                                                                                    ( Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Just
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , value = "e"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'FunctionCall
                                                                                                                        ( FunctionCallExpression
                                                                                                                            { call = FunctionCall
                                                                                                                                { name = Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "coalesce"
                                                                                                                                    }
                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                    [ Expression'Subquery
                                                                                                                                        ( SelectStatement
                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                            , select = CompoundSelect
                                                                                                                                                ( SelectCore'Select
                                                                                                                                                    ( Select
                                                                                                                                                        { distinct = False
                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                            ( Aliased
                                                                                                                                                                { value = Expression'Plus
                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "a"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            , value = "c"
                                                                                                                                                                            }
                                                                                                                                                                        )
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
                                                                                                                                                            ( Expression'InSubquery
                                                                                                                                                                ( InSubqueryExpression
                                                                                                                                                                    { expression = Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "d"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , subquery = SelectStatement
                                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                                        , select = Union
                                                                                                                                                                            ( CompoundSelect
                                                                                                                                                                                ( SelectCore'Select
                                                                                                                                                                                    ( Select
                                                                                                                                                                                        { distinct = False
                                                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                                                            ( Aliased
                                                                                                                                                                                                { value = Expression'Plus
                                                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "min"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                    [ Expression'Column
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            , value = "f"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                }
                                                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                                                            , over = Nothing
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
                                                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "count"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                , arguments = FunctionArguments'Wildcard
                                                                                                                                                                                                                }
                                                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                                                            , over = Nothing
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
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
                                                                                                                                                                                        , where_ = Nothing
                                                                                                                                                                                        , groupBy = Nothing
                                                                                                                                                                                        , window = Nothing
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                )
                                                                                                                                                                            )
                                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                                ( Select
                                                                                                                                                                                    { distinct = False
                                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                                        ( Aliased
                                                                                                                                                                                            { value = Expression'AggregateDistinctFunctionCall
                                                                                                                                                                                                ( AggregateDistinctFunctionCallExpression
                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "count"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        , arguments = Identity
                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    , value = "e"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            )
                                                                                                                                                                                                        }
                                                                                                                                                                                                    , filter = Nothing
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
                                                                                                                                                                                    , where_ = Nothing
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
                                                                                                                                                        , groupBy = Nothing
                                                                                                                                                        , window = Nothing
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                            , orderBy = Nothing
                                                                                                                                            , limit = Nothing
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    , Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Just
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "t1"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            , value = "f"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    ]
                                                                                                                                }
                                                                                                                            , filter = Nothing
                                                                                                                            , over = Nothing
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                , Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Just
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "t1"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , value = "c"
                                                                                                                        }
                                                                                                                    )
                                                                                                                ) :| []
                                                                                                            , else_ = Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "d"
                                                                                                                    }
                                                                                                                )
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
                                                                                                ( Expression'Or
                                                                                                    ( Expression'InSubquery
                                                                                                        ( InSubqueryExpression
                                                                                                            { expression = Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Just
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "t1"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , value = "f"
                                                                                                                    }
                                                                                                                )
                                                                                                            , subquery = SelectStatement
                                                                                                                { commonTableExpressions = Nothing
                                                                                                                , select = Union
                                                                                                                    ( CompoundSelect
                                                                                                                        ( SelectCore'Select
                                                                                                                            ( Select
                                                                                                                                { distinct = False
                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                    ( Aliased
                                                                                                                                        { value = Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Just
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "t1"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                , value = "f"
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
                                                                                                                                , where_ = Nothing
                                                                                                                                , groupBy = Nothing
                                                                                                                                , window = Nothing
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    )
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
                                                                                                                            , where_ = Nothing
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
                                                                                                    ( Expression'Not
                                                                                                        ( Expression'InValues
                                                                                                            ( InValuesExpression
                                                                                                                { expression = Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Just
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "t1"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , value = "a"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , values =
                                                                                                                    [ Expression'Negate
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    , Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Just
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , value = "a"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Just
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , value = "b"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    ]
                                                                                                                }
                                                                                                            )
                                                                                                        )
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
                                                                            )
                                                                        , Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Just
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "t1"
                                                                                        }
                                                                                    )
                                                                                , value = "c"
                                                                                }
                                                                            )
                                                                        ]
                                                                    }
                                                                , filter = Nothing
                                                                , over = Nothing
                                                                }
                                                            )
                                                        ) :| []
                                                    , else_ = Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "t1"
                                                                    }
                                                                )
                                                            , value = "f"
                                                            }
                                                        )
                                                    }
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "f"
                                                    }
                                                )
                                            )
                                        ) :|
                                        [
                                            ( Expression'Not
                                                ( Expression'Exists
                                                    ( SelectStatement
                                                        { commonTableExpressions = Nothing
                                                        , select = CompoundSelect
                                                            ( SelectCore'Select
                                                                ( Select
                                                                    { distinct = False
                                                                    , columns = ResultColumn'Expression
                                                                        ( Aliased
                                                                            { value = Expression'LiteralValue
                                                                                ( Number "1" )
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
                                                                        ( Expression'Between
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "b"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "f"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "t1"
                                                                                            }
                                                                                        )
                                                                                    , value = "a"
                                                                                    }
                                                                                )
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
                                                    )
                                                )
                                            , Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "c"
                                                    }
                                                )
                                            )
                                        ]
                                    , else_ = Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "t1"
                                                    }
                                                )
                                            , value = "f"
                                            }
                                        )
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
                        ( Expression'LessThanOrEquals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "t1"
                                            }
                                        )
                                    , value = "a"
                                    }
                                )
                            )
                            ( Expression'Plus
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "coalesce"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Subquery
                                                    ( SelectStatement
                                                        { commonTableExpressions = Nothing
                                                        , select = CompoundSelect
                                                            ( SelectCore'Select
                                                                ( Select
                                                                    { distinct = False
                                                                    , columns = ResultColumn'Expression
                                                                        ( Aliased
                                                                            { value = Expression'FunctionCall
                                                                                ( FunctionCallExpression
                                                                                    { call = FunctionCall
                                                                                        { name = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "max"
                                                                                            }
                                                                                        , arguments = FunctionArguments'Arguments
                                                                                            [ Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "b"
                                                                                                    }
                                                                                                )
                                                                                            ]
                                                                                        }
                                                                                    , filter = Nothing
                                                                                    , over = Nothing
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
                                                                            ( Expression'FunctionCall
                                                                                ( FunctionCallExpression
                                                                                    { call = FunctionCall
                                                                                        { name = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "coalesce"
                                                                                            }
                                                                                        , arguments = FunctionArguments'Arguments
                                                                                            [ Expression'Subquery
                                                                                                ( SelectStatement
                                                                                                    { commonTableExpressions = Nothing
                                                                                                    , select = CompoundSelect
                                                                                                        ( SelectCore'Select
                                                                                                            ( Select
                                                                                                                { distinct = False
                                                                                                                , columns = ResultColumn'Expression
                                                                                                                    ( Aliased
                                                                                                                        { value = Expression'Minus
                                                                                                                            ( Expression'Multiply
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Just
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "t1"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        , value = "c"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                                ( Expression'Case
                                                                                                                                    ( CaseExpression
                                                                                                                                        { base = Nothing
                                                                                                                                        , cases =
                                                                                                                                            ( Expression'Equals
                                                                                                                                                ( Expression'FunctionCall
                                                                                                                                                    ( FunctionCallExpression
                                                                                                                                                        { call = FunctionCall
                                                                                                                                                            { name = Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "coalesce"
                                                                                                                                                                }
                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                [ Expression'Subquery
                                                                                                                                                                    ( SelectStatement
                                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                                        , select = CompoundSelect
                                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                                ( Select
                                                                                                                                                                                    { distinct = False
                                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                                        ( Aliased
                                                                                                                                                                                            { value = Expression'FunctionCall
                                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "max"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                            [ Expression'Subquery
                                                                                                                                                                                                                ( SelectStatement
                                                                                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                                                                                            ( Select
                                                                                                                                                                                                                                { distinct = False
                                                                                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                                                                                    ( Aliased
                                                                                                                                                                                                                                        { value = Expression'FunctionCall
                                                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                        , value = "count"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    , arguments = FunctionArguments'Wildcard
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                , filter = Nothing
                                                                                                                                                                                                                                                , over = Nothing
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
                                                                                                                                                                                                                                , where_ = Nothing
                                                                                                                                                                                                                                , groupBy = Nothing
                                                                                                                                                                                                                                , window = Nothing
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    , orderBy = Nothing
                                                                                                                                                                                                                    , limit = Nothing
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            ]
                                                                                                                                                                                                        }
                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                    , over = Nothing
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
                                                                                                                                                                                        ( Expression'Not
                                                                                                                                                                                            ( Expression'Not
                                                                                                                                                                                                ( Expression'Exists
                                                                                                                                                                                                    ( SelectStatement
                                                                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                                                                        , select = CompoundSelect
                                                                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                                                                ( Select
                                                                                                                                                                                                                    { distinct = False
                                                                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                                                                        ( Aliased
                                                                                                                                                                                                                            { value = Expression'LiteralValue
                                                                                                                                                                                                                                ( Number "1" )
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
                                                                                                                                                                                                                        ( Expression'And
                                                                                                                                                                                                                            ( Expression'And
                                                                                                                                                                                                                                ( Expression'NotEquals
                                                                                                                                                                                                                                    ( Expression'Case
                                                                                                                                                                                                                                        ( CaseExpression
                                                                                                                                                                                                                                            { base = Just
                                                                                                                                                                                                                                                ( Expression'BitwiseOr
                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                            , value = "a"
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            , cases =
                                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                ) :| []
                                                                                                                                                                                                                                            , else_ = Expression'LiteralValue
                                                                                                                                                                                                                                                ( Number "11" )
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                            , value = "f"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                ( Expression'GreaterThanOrEquals
                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                            ( Expression'Between
                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
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
                                                                                                                                                                                                    )
                                                                                                                                                                                                )
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
                                                                                                                                                                    )
                                                                                                                                                                , Expression'LiteralValue
                                                                                                                                                                    ( Number "13" )
                                                                                                                                                                ]
                                                                                                                                                            }
                                                                                                                                                        , filter = Nothing
                                                                                                                                                        , over = Nothing
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'Case
                                                                                                                                                    ( CaseExpression
                                                                                                                                                        { base = Just
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "e"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        , cases =
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "a"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            , Expression'LiteralValue
                                                                                                                                                                ( Number "19" )
                                                                                                                                                            ) :| []
                                                                                                                                                        , else_ = Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "d"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                            , Expression'Column
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Just
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "t1"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    , value = "b"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            ) :|
                                                                                                                                            [
                                                                                                                                                ( Expression'Not
                                                                                                                                                    ( Expression'InValues
                                                                                                                                                        ( InValuesExpression
                                                                                                                                                            { expression = Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Just
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "t1"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , value = "a"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            , values =
                                                                                                                                                                [ Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "a"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Just
                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        , value = "b"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "a"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                ]
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                , Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "e"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                            ]
                                                                                                                                        , else_ = Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "b"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            )
                                                                                                                            ( Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "d"
                                                                                                                                    }
                                                                                                                                )
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
                                                                                                                    ( Expression'Between
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "f"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "d"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "e"
                                                                                                                                }
                                                                                                                            )
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
                                                                                                )
                                                                                            , Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "e"
                                                                                                    }
                                                                                                )
                                                                                            ]
                                                                                        }
                                                                                    , filter = Nothing
                                                                                    , over = Nothing
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "t1"
                                                                                            }
                                                                                        )
                                                                                    , value = "f"
                                                                                    }
                                                                                )
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
                                                    )
                                                , Expression'LiteralValue
                                                    ( Number "17" )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "t1"
                                                }
                                            )
                                        , value = "f"
                                        }
                                    )
                                )
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
    )