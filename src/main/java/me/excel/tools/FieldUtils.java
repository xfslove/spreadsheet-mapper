package me.excel.tools;

import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static me.excel.tools.ExcelConstants.BUSINESS_KEY_PREFIX;
import static me.excel.tools.ExcelConstants.DOT_SEPARATOR;

/**
 * model field utils
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class FieldUtils {

  private FieldUtils() {
    // default constructor
  }

  /**
   * get field with out business key
   *
   * @param field field
   * @return field without business key
   */
  public static String subtractBusinessKey(String field) {
    if (!StringUtils.contains(field, BUSINESS_KEY_PREFIX)) {
      throw new IllegalStateException("field is not business key");
    }
    return field.substring(BUSINESS_KEY_PREFIX.length());
  }

  /**
   * <pre>
   * get field name without prefix
   * eg:
   * model.name -&gt; name
   * model.nested.name -&gt; nested.name
   * </pre>
   *
   * @param field field
   * @return field name of model
   */
  public static String detectRealField(String field) {

    String realField = field;

    if (field.contains(BUSINESS_KEY_PREFIX)) {
      realField = FieldUtils.subtractBusinessKey(field);
    }

    List<String> splitFields = new ArrayList<>(Arrays.asList(realField.split("\\.")));
    if (splitFields.size() == 1) {
      return realField;
    } else {
      splitFields.remove(0);
      return StringUtils.join(splitFields, DOT_SEPARATOR);
    }
  }

  /**
   * <pre>
   * get field if can't find return null.
   * supported private, private final, protected, protected final, public, public final.
   * </pre>
   *
   * @param clazz clazz
   * @param field field name of model
   * @return {@link Field}
   */
  public static Field getField(Class clazz, String field) {
    try {
      return clazz.getDeclaredField(field);
    } catch (NoSuchFieldException e) {
      if (clazz.getSuperclass() != null && !clazz.getSuperclass().equals(Object.class)) {
        return getField(clazz.getSuperclass(), field);
      }
    }
    return null;
  }

  /**
   * get field type if can't find return null.
   *
   * @param clazz  clazz
   * @param fields eg: [objectB, objectA, name] is objectB.objectA.name
   * @return {@link Field#getType()}
   */
  public static Class getFieldType(Class clazz, String[] fields) {
    if (fields.length == 0 || clazz == null) {
      return null;
    }
    if (fields.length > 1) {
      String[] newFields = new String[fields.length - 1];
      System.arraycopy(fields, 1, newFields, 0, newFields.length);
      return getFieldType(getFieldType(clazz, fields[0]), newFields);
    }
    return getFieldType(clazz, fields[0]);
  }

  /**
   * @param clazz     clazz
   * @param fieldName field name of model
   * @return {@link Field#getType()}
   * @see #getFieldType(Class, String[])
   */
  public static Class getFieldType(Class clazz, String fieldName) {

    Field field = getField(clazz, fieldName);

    if (field == null) {
      return null;
    }

    return field.getType();
  }
}
