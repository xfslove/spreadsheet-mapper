package spreadsheet.mapper.o2w.compose.builder;

import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.FieldMetaBean;
import spreadsheet.mapper.model.meta.HeaderMeta;
import spreadsheet.mapper.model.meta.HeaderMetaBean;

/**
 * {@link FieldMeta} builder
 * <p>
 * Created by hanwen on 2017/1/10.
 */
public class SequenceBasedFieldMetaBuilder {

  private int rowIndex = 1;
  private FieldMeta fieldMeta;
  private SequenceBasedSheetMetaBuilder sequenceBasedSheetMetaBuilder;

  SequenceBasedFieldMetaBuilder(String prefix, String name, int columnIndex) {
    this.fieldMeta = new FieldMetaBean(prefix, name, columnIndex);
  }

  /**
   * create a header meta and add at {@link #rowIndex}, after add {@link #rowIndex} will plus 1
   *
   * @param value {@link HeaderMeta#getValue()}
   * @return this
   */
  public SequenceBasedFieldMetaBuilder header(String value) {
    headers(value);
    return this;
  }

  /**
   * add header meta by sequence
   *
   * @param values {@link HeaderMeta#getValue()}
   * @return this
   * @see #header(String)
   */
  public SequenceBasedFieldMetaBuilder headers(String... values) {
    if (values == null) {
      return this;
    }
    for (String value : values) {
      fieldMeta.addHeaderMeta(new HeaderMetaBean(rowIndex, value));
      rowIndex++;
    }
    return this;
  }

  /**
   * skip one row
   *
   * @return this
   */
  public SequenceBasedFieldMetaBuilder skip() {
    skip(1);
    return this;
  }

  /**
   * skip supplied numbers rows
   *
   * @param rowNum skip how much columns
   * @return this
   */
  public SequenceBasedFieldMetaBuilder skip(int rowNum) {
    rowIndex += rowNum;
    return this;
  }

  /**
   * finish one field create, go to next
   *
   * @return {@link SequenceBasedSheetMetaBuilder}
   */
  public SequenceBasedSheetMetaBuilder next() {
    sequenceBasedSheetMetaBuilder.rowIndex = Math.max(this.rowIndex, sequenceBasedSheetMetaBuilder.rowIndex);
    sequenceBasedSheetMetaBuilder.fieldMetas.add(fieldMeta);
    return sequenceBasedSheetMetaBuilder;
  }

  void setSequenceBasedSheetMetaBuilder(SequenceBasedSheetMetaBuilder sequenceBasedSheetMetaBuilder) {
    this.sequenceBasedSheetMetaBuilder = sequenceBasedSheetMetaBuilder;
  }
}
