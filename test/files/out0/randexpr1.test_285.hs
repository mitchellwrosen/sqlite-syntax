Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseNegate
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
                                            ( Expression'And
                                                ( Expression'And
                                                    ( Expression'GreaterThan
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
                                                        ( Expression'Divide
                                                            ( Expression'FunctionCall
                                                                ( FunctionCallExpression
                                                                    { call = FunctionCall
                                                                        { name = Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "abs"
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
                                                                                    , value = "c"
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
                                                                            , value = "abs"
                                                                            }
                                                                        , arguments = FunctionArguments'Arguments
                                                                            [ Expression'Plus
                                                                                ( Expression'Minus
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "13" )
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
                                                                                                                                                        { namespace = Just
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "t1"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        , value = "a"
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
                                                                                                                            ( Expression'InSubquery
                                                                                                                                ( InSubqueryExpression
                                                                                                                                    { expression = Expression'Case
                                                                                                                                        ( CaseExpression
                                                                                                                                            { base = Nothing
                                                                                                                                            , cases =
                                                                                                                                                ( Expression'Not
                                                                                                                                                    ( Expression'InValues
                                                                                                                                                        ( InValuesExpression
                                                                                                                                                            { expression = Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "e"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            , values =
                                                                                                                                                                [ Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "c"
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
                                                                                                                                                                        , value = "a"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , Expression'BitwiseNegate
                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                        ( Number "11" )
                                                                                                                                                                    )
                                                                                                                                                                ]
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                , Expression'Plus
                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                        ( Number "19" )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Negate
                                                                                                                                                        ( Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "c"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                ) :| []
                                                                                                                                            , else_ = Expression'Column
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "a"
                                                                                                                                                    }
                                                                                                                                                )
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
                                                                                                                                                                { value = Expression'LiteralValue
                                                                                                                                                                    ( Number "19" )
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
                                                                                                                                                                    { namespace = Just
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "t1"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , value = "c"
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
                                                                                                            , value = "b"
                                                                                                            }
                                                                                                        )
                                                                                                    ]
                                                                                                }
                                                                                            , filter = Nothing
                                                                                            , over = Nothing
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "a"
                                                                                        }
                                                                                    )
                                                                                )
                                                                            ]
                                                                        }
                                                                    , filter = Nothing
                                                                    , over = Nothing
                                                                    }
                                                                )
                                                            )
                                                        )
                                                    )
                                                    ( Expression'GreaterThan
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "e"
                                                                }
                                                            )
                                                        )
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "d"
                                                                }
                                                            )
                                                        )
                                                    )
                                                )
                                                ( Expression'Equals
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
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "e"
                                                            }
                                                        )
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
                                                    , value = "e"
                                                    }
                                                )
                                            ) :|
                                            [
                                                ( Expression'Not
                                                    ( Expression'And
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
                                                                                ( Expression'Equals
                                                                                    ( Expression'Negate
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
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
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
                                                        )
                                                        ( Expression'InSubquery
                                                            ( InSubqueryExpression
                                                                { expression = Expression'Negate
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "d"
                                                                            }
                                                                        )
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
                                                                                                    { namespace = Nothing
                                                                                                    , value = "d"
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
                                                                                                , value = "e"
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
                                                )
                                            ]
                                        , else_ = Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
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
                                                            { value = Expression'FunctionCall
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
                                                                                                                        [ Expression'FunctionCall
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
                                                                                                                                                                    { value = Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "c"
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
                                                                                                                                                                    ( Expression'InValues
                                                                                                                                                                        ( InValuesExpression
                                                                                                                                                                            { expression = Expression'FunctionCall
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
                                                                                                                                                                                                                                        [ Expression'FunctionCall
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
                                                                                                                                                                                                                                                                                                    [ Expression'Multiply
                                                                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                                                                            ( Number "13" )
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
                                                                                                                                                                                                                                                                                ( Expression'LessThan
                                                                                                                                                                                                                                                                                    ( Expression'Case
                                                                                                                                                                                                                                                                                        ( CaseExpression
                                                                                                                                                                                                                                                                                            { base = Nothing
                                                                                                                                                                                                                                                                                            , cases =
                                                                                                                                                                                                                                                                                                ( Expression'Or
                                                                                                                                                                                                                                                                                                    ( Expression'Not
                                                                                                                                                                                                                                                                                                        ( Expression'InValues
                                                                                                                                                                                                                                                                                                            ( InValuesExpression
                                                                                                                                                                                                                                                                                                                { expression = Expression'Case
                                                                                                                                                                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                                                                                                                                                                        { base = Nothing
                                                                                                                                                                                                                                                                                                                        , cases =
                                                                                                                                                                                                                                                                                                                            ( Expression'Or
                                                                                                                                                                                                                                                                                                                                ( Expression'NotEquals
                                                                                                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                                                                                ( Expression'LessThanOrEquals
                                                                                                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                                                    ( Expression'Negate
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
                                                                                                                                                                                                                                                                                                                                                [ Expression'Negate
                                                                                                                                                                                                                                                                                                                                                    ( Expression'Negate
                                                                                                                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                                                                                                                            ( Number "13" )
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
                                                                                                                                                                                                                                                                                                                                                , Expression'LiteralValue
                                                                                                                                                                                                                                                                                                                                                    ( Number "13" )
                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                                                                                                        , value = "a"
                                                                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                        , else_ = Expression'Column
                                                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                                                , value = "c"
                                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                                , values =
                                                                                                                                                                                                                                                                                                                    [ Expression'Negate
                                                                                                                                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                                                , value = "e"
                                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                                    , Expression'Column
                                                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                                                            , value = "d"
                                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                                                                                                                                                                                        ( Number "11" )
                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                    ( Expression'LessThan
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
                                                                                                                                                                                                                                                                                                                , value = "a"
                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                            )
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
                                                                                                                                                                                                                                                                                                    ( Expression'NotEquals
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
                                                                                                                                                                                                                                                                                                        ( Expression'Negate
                                                                                                                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                                                                    , value = "b"
                                                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                                                )
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
                                                                                                                                                                                                                                                                                                            , value = "e"
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
                                                                                                                                                                                                                                                                                                    , value = "d"
                                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                                )
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
                                                                                                                                                                                                                                                                , value = "d"
                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                , filter = Nothing
                                                                                                                                                                                                                                                , over = Nothing
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
                                                                                                                                                                                                                    ( Expression'NotEquals
                                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                , value = "e"
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
                                                                                                                                                                                                    , value = "c"
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            ]
                                                                                                                                                                                        }
                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            , values =
                                                                                                                                                                                [ Expression'Column
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
                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                ]
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
                                                                                                                                            ( Number "13" )
                                                                                                                                        ]
                                                                                                                                    }
                                                                                                                                , filter = Nothing
                                                                                                                                , over = Nothing
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
                                                                                                                                                                , value = "a"
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
                                                                                                                                    { value = Expression'FunctionCall
                                                                                                                                        ( FunctionCallExpression
                                                                                                                                            { call = FunctionCall
                                                                                                                                                { name = Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "abs"
                                                                                                                                                    }
                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                    [ Expression'FunctionCall
                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                { name = Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "abs"
                                                                                                                                                                    }
                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                    [ Expression'FunctionCall
                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "min"
                                                                                                                                                                                    }
                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                    [ Expression'LiteralValue
                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                    ]
                                                                                                                                                                                }
                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                            , over = Nothing
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    ]
                                                                                                                                                                }
                                                                                                                                                            , filter = Nothing
                                                                                                                                                            , over = Nothing
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
                                                                            , Expression'LiteralValue
                                                                                ( Number "11" )
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
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )