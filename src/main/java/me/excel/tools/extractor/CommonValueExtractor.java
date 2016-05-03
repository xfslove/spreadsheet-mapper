package me.excel.tools.extractor;

import java.util.function.BiFunction;

/**
 * Created by hanwen on 5/3/16.
 */
public class CommonValueExtractor extends AbstractCellValueExtractor {

  protected BiFunction<Object, String, String> stringValueGetter;

  public CommonValueExtractor(String matchField, BiFunction<Object, String, String> stringValueGetter) {
    super(matchField);
    this.stringValueGetter = stringValueGetter;
  }

  @Override
  public String getStringValue(Object data, String field) {
    if (stringValueGetter == null) {
      return null;
    }
    return stringValueGetter.apply(data, field);
  }
}
