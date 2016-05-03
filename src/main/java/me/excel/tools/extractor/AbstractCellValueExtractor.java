package me.excel.tools.extractor;

import me.excel.tools.FieldUtils;

import static me.excel.tools.FieldUtils.getFieldWithoutPrefix;

/**
 * Created by hanwen on 5/3/16.
 */
public abstract class AbstractCellValueExtractor implements CellValueExtractor {

  private String matchField;

  public AbstractCellValueExtractor(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public abstract String getStringValue(Object data, String field);

  @Override
  public boolean matches(String field) {
    return field.equals(matchField);
  }

  protected String getFiledWithOutPrefix(String field) {
    String fieldWithoutPrefix;

    if (field.contains(FieldUtils.BUSINESS_KEY_PREFIX)) {
      fieldWithoutPrefix = getFieldWithoutPrefix(FieldUtils.getBusinessKeyField(field));
    } else {
      fieldWithoutPrefix = getFieldWithoutPrefix(field);
    }

    return fieldWithoutPrefix;
  }
}
