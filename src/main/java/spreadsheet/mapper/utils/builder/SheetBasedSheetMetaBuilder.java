package spreadsheet.mapper.utils.builder;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * {@link SheetMeta} builder, build from supplied {@link #sheet(Sheet)}
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class SheetBasedSheetMetaBuilder implements SheetMetaBuilder {

  private Sheet sheet;

  private Integer dataStartRowIndex;

  private Integer fieldRowIndex;

  private List<String> fieldPrefixes = new ArrayList<>();

  private List<Integer> headerRowIndices = new ArrayList<>();

  /**
   * @param sheet from sheet
   * @return {@link SheetBasedSheetMetaBuilder}
   */
  public SheetBasedSheetMetaBuilder sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  /**
   * @param dataStartRowIndex {@link SheetMeta#getDataStartRowIndex()}
   * @return {@link SheetBasedSheetMetaBuilder}
   */
  public SheetBasedSheetMetaBuilder dataStartRowIndex(int dataStartRowIndex) {
    this.dataStartRowIndex = dataStartRowIndex;
    return this;
  }

  /**
   * which row is field row at {@link #sheet}
   *
   * @param fieldRowIndex 1-based
   * @return {@link SheetBasedSheetMetaBuilder}
   */
  public SheetBasedSheetMetaBuilder fieldRowIndex(int fieldRowIndex) {
    this.fieldRowIndex = fieldRowIndex;
    return this;
  }

  /**
   * exists which field prefixes of fields at sheet's field row {@link #fieldRowIndex(int)}
   *
   * @param prefixes {@link FieldMeta#getPrefix()}
   * @return {@link SheetBasedSheetMetaBuilder}
   */
  public SheetBasedSheetMetaBuilder fieldPrefixes(String... prefixes) {
    if (prefixes == null) {
      return this;
    }
    Collections.addAll(this.fieldPrefixes, prefixes);
    return this;
  }

  /**
   * if you want specify the field row is header, should pass {@link #headerRowIndices} with {@link #fieldRowIndex}
   *
   * @param headerRowIndices header row indices
   * @return {@link SheetBasedSheetMetaBuilder}
   */
  public SheetBasedSheetMetaBuilder headerRowIndices(int... headerRowIndices) {
    if (headerRowIndices == null) {
      return this;
    }
    for (int headerRowIndex : headerRowIndices) {
      this.headerRowIndices.add(headerRowIndex);
    }
    return this;
  }

  @Override
  public SheetMeta toSheetMeta() {
    if (sheet == null) {
      throw new IllegalArgumentException("set sheet first");
    }
    if (dataStartRowIndex == null) {
      throw new IllegalArgumentException("set data start row index first");
    }
    if (fieldRowIndex == null) {
      throw new IllegalArgumentException("set field row index first");
    }

    if (dataStartRowIndex <= fieldRowIndex) {
      throw new IllegalArgumentException("data start row index must be greater than field row index[" + fieldRowIndex + "]");
    }

    if (CollectionUtils.isNotEmpty(headerRowIndices)) {

      Collections.sort(headerRowIndices);
      int maxHeaderRowIndex = headerRowIndices.get(headerRowIndices.size() - 1);
      if (dataStartRowIndex <= maxHeaderRowIndex) {
        throw new IllegalArgumentException("data start row index must be greater than max header row index[" + maxHeaderRowIndex + "]");
      }
    }

    SheetMeta sheetMeta = new SheetMetaBean(sheet.getName(), dataStartRowIndex);

    Row fieldRow = sheet.getRow(fieldRowIndex);

    for (int i = 1; i <= fieldRow.sizeOfCells(); i++) {

      String fieldName = fieldRow.getCell(i).getValue();

      if (StringUtils.isBlank(fieldName)) {
        continue;
      }

      FieldMeta fieldMeta = null;
      for (String fieldPrefix : fieldPrefixes) {
        if (StringUtils.startsWith(fieldName, fieldPrefix)) {
          fieldMeta = new FieldMetaBean(fieldPrefix, fieldName, i);
          break;
        }
      }

      if (fieldMeta == null) {
        fieldMeta = new FieldMetaBean(fieldName, i);
      }

      for (int headerRowIndex : headerRowIndices) {
        Cell headerCell = sheet.getRow(headerRowIndex).getCell(i);
        if (StringUtils.isBlank(headerCell.getValue())) {
          continue;
        }

        HeaderMeta headerMeta = new HeaderMetaBean(headerRowIndex, headerCell.getValue());
        fieldMeta.addHeaderMeta(headerMeta);
      }

      sheetMeta.addFieldMeta(fieldMeta);
    }

    return sheetMeta;
  }

}
