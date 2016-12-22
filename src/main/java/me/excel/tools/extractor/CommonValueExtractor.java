package me.excel.tools.extractor;

import java.util.function.Function;

/**
 * customer value extractor
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class CommonValueExtractor<D> extends FieldValueExtractorAdapter {

  private Function<D, String> stringValueGetter;

  public CommonValueExtractor(String matchField, Function<D, String> stringValueGetter) {
    super(matchField);
    this.stringValueGetter = stringValueGetter;
  }

  @Override
  public String getStringValue(Object data) {
    if (stringValueGetter == null) {
      return null;
    }
    return stringValueGetter.apply((D) data);
  }
}
