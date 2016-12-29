package excel.engine.model.ext;

import excel.engine.exception.ExcelTemplateException;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;

import java.util.*;

/**
 * Created by hanwen on 2016/12/28.
 */
public class SheetContextBean implements SheetContext {

  private int sheetIndex;

  private String sheetName;

  private List<String> fields = new ArrayList<>();

  private Integer dataStartRowIndex;

  private List<Object> data = new ArrayList<>();

  private Map<Integer, SheetHeader> rowIndex2sheetHeader = new HashMap<>();

  public SheetContextBean(String... fields) {
    this(1, null, fields);
  }

  public SheetContextBean(int sheetIndex, String... fields) {
    this(sheetIndex, null, fields);
  }

  /**
   * @param sheetIndex which sheet
   * @param sheetName  sheet name
   * @param fields     fields
   * @see SheetContext#getFields()
   */
  public SheetContextBean(int sheetIndex, String sheetName, String... fields) {
    if (fields == null) {
      throw new ExcelTemplateException("fields can not be null");
    }
    this.sheetIndex = sheetIndex;
    this.sheetName = sheetName;
    Collections.addAll(this.fields, fields);
  }

  @Override
  public void addHeader(HeaderMeta meta, String... values) {
    if (values.length != fields.size()) {
      throw new ExcelTemplateException("supplied values size not equals fields size, supplied values can not corresponding to field");
    }

    SheetHeader sheetHeader = new SheetHeaderBean(meta);
    for (int i = 0; i < fields.size(); i++) {
      sheetHeader.addValue(fields.get(i), values[i]);
    }
    rowIndex2sheetHeader.put(sheetHeader.getHeaderMeta().getRowIndex(), sheetHeader);
  }

  @Override
  public void addHeader(SheetHeader sheetHeader) {
    rowIndex2sheetHeader.put(sheetHeader.getHeaderMeta().getRowIndex(), sheetHeader);
  }

  @Override
  public void setData(List<Object> data) {
    this.data = data;
  }

  @Override
  public void setData(int dataStartRowIndex, List<Object> data) {
    this.dataStartRowIndex = dataStartRowIndex;
    this.data = data;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public String getSheetName() {
    return sheetName;
  }

  @Override
  public int getDataStartRowIndex() {
    int maxHeaderRowIndex = getMaxHeaderRowIndex();
    if (dataStartRowIndex == null) {
      return maxHeaderRowIndex;
    }
    if (maxHeaderRowIndex >= dataStartRowIndex) {
      throw new ExcelTemplateException("data row[" + dataStartRowIndex + "] must after max header row[" + maxHeaderRowIndex + "]");
    }
    return dataStartRowIndex;
  }

  @Override
  public List<Object> getData() {
    return data;
  }

  @Override
  public SheetTemplate ofTemplate() {
    int dataStartRowIndex = getDataStartRowIndex();

    if (MapUtils.isEmpty(rowIndex2sheetHeader)) {
      return new SheetTemplateBean(sheetIndex, dataStartRowIndex);
    }

    List<HeaderMeta> metas = new ArrayList<>();
    for (SheetHeader sheetHeader : rowIndex2sheetHeader.values()) {
      metas.add(sheetHeader.getHeaderMeta());
    }

    return new SheetTemplateBean(sheetIndex, dataStartRowIndex, metas.toArray(new HeaderMeta[0]));
  }

  @Override
  public List<String> getFields() {
    return fields;
  }

  @Override
  public List<SheetHeader> getSheetHeaders() {
    return new ArrayList<>(rowIndex2sheetHeader.values());
  }

  private int getMaxHeaderRowIndex() {
    int maxRowIndex = 0;
    for (Integer rowIndex : rowIndex2sheetHeader.keySet()) {
      maxRowIndex = Math.max(rowIndex, maxRowIndex);
    }
    return maxRowIndex + 1;
  }
}
