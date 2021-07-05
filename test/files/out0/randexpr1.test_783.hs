Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Minus
                                ( Expression'Plus
                                    ( Expression'Minus
                                        ( Expression'Plus
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "d"
                                                    }
                                                )
                                            )
                                            ( Expression'Multiply
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
                                                        , value = "c"
                                                        }
                                                    )
                                                )
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
                                                                            ( Expression'GreaterThanOrEquals
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "c"
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
                                                    , Expression'Case
                                                        ( CaseExpression
                                                            { base = Nothing
                                                            , cases =
                                                                ( Expression'InSubquery
                                                                    ( InSubqueryExpression
                                                                        { expression = Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "c"
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
                                                                                )
                                                                                ( SelectCore'Select
                                                                                    ( Select
                                                                                        { distinct = False
                                                                                        , columns = ResultColumn'Expression
                                                                                            ( Aliased
                                                                                                { value = Expression'LiteralValue
                                                                                                    ( Number "13" )
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
                                                                , Expression'Plus
                                                                    ( Expression'LiteralValue
                                                                        ( Number "11" )
                                                                    )
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "a"
                                                                            }
                                                                        )
                                                                    )
                                                                ) :| []
                                                            , else_ = Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "f"
                                                                    }
                                                                )
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
                                ( Expression'LiteralValue
                                    ( Number "13" )
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
                            ( Expression'Between
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "c"
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
                                ( Expression'Plus
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
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
                                                                                ( Expression'BitwiseOr
                                                                                    ( Expression'Plus
                                                                                        ( Expression'Case
                                                                                            ( CaseExpression
                                                                                                { base = Nothing
                                                                                                , cases =
                                                                                                    ( Expression'Not
                                                                                                        ( Expression'InValues
                                                                                                            ( InValuesExpression
                                                                                                                { expression = Expression'LiteralValue
                                                                                                                    ( Number "13" )
                                                                                                                , values =
                                                                                                                    [ Expression'Case
                                                                                                                        ( CaseExpression
                                                                                                                            { base = Just
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "a"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            , cases =
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "b"
                                                                                                                                        }
                                                                                                                                    )
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
                                                                                                                                                                            { value = Expression'FunctionCall
                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "max"
                                                                                                                                                                                            }
                                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                                            [ Expression'Multiply
                                                                                                                                                                                                ( Expression'Case
                                                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                                                        { base = Nothing
                                                                                                                                                                                                        , cases =
                                                                                                                                                                                                            ( Expression'InSubquery
                                                                                                                                                                                                                ( InSubqueryExpression
                                                                                                                                                                                                                    { expression = Expression'Column
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
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
                                                                                                                                                                                                                                                { value = Expression'Case
                                                                                                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                                                                                                        { base = Nothing
                                                                                                                                                                                                                                                        , cases =
                                                                                                                                                                                                                                                            ( Expression'Not
                                                                                                                                                                                                                                                                ( Expression'Between
                                                                                                                                                                                                                                                                    ( Expression'BitwiseOr
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
                                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                            , value = "c"
                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            , Expression'Minus
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
                                                                                                                                                                                                                                                            ) :| []
                                                                                                                                                                                                                                                        , else_ = Expression'LiteralValue
                                                                                                                                                                                                                                                            ( Number "17" )
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
                                                                                                                                                                                                                                            { value = Expression'LiteralValue
                                                                                                                                                                                                                                                ( Number "13" )
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
                                                                                                                                                                                                            ) :| []
                                                                                                                                                                                                        , else_ = Expression'Column
                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                , value = "e"
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
                                                                                                                                                                        ( Expression'Equals
                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                ( Number "11" )
                                                                                                                                                                            )
                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                ( Number "17" )
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
                                                                                                                                ) :| []
                                                                                                                            , else_ = Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "f"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , Expression'LiteralValue
                                                                                                                        ( Number "17" )
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
                                                                                                            , value = "d"
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
                                                                                                        , value = "d"
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
                                                                                                , value = "a"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "b"
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
                                                            { namespace = Nothing
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