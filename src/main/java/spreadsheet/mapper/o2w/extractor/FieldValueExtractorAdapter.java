package spreadsheet.mapper.o2w.extractor;

import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * field value extractor adapter, easy implements customer value extractor extends this.
 * <p>
 * Created by hanwen on 5/3/16.
 */
public abstract class FieldValueExtractorAdapter<T> implements FieldValueExtractor<T> {

  private String matchField;

  public FieldValueExtractorAdapter(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public abstract String getStringValue(T object, FieldMeta fieldMeta);
}
